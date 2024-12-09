use crate::*;
use foldhash::HashMap;
use image::{GenericImageView, ImageFormat, RgbaImage};
use ldtk2::Ldtk;
use proc_macro2::Span;
use quote::{quote, TokenStreamExt};
use std::{
    error::Error,
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
};
use syn::Ident;

pub fn build_all_ldtk_scenes<P: AsRef<Path>>(path: P) -> Result<(), Box<dyn Error>> {
    for entry in walkdir::WalkDir::new(path) {
        let entry = entry.unwrap();
        let path = entry.path();

        if path.extension().is_some_and(|e| e == "ldtk") {
            build_ldtk_scene(path)
                .unwrap_or_else(|e| panic!("Unable to build scene from LDtk file {path:?}: {e}"));
        }
    }

    Ok(())
}

/// Builds a deserialized LDtk scene from a given file.
pub fn build_ldtk_scene<P: AsRef<Path>>(path: P) -> Result<(), Box<dyn Error>> {
    let world_name = path
        .as_ref()
        .file_stem()
        .expect("invalid path")
        .to_str()
        .expect("invalid unicode");
    let map = Ldtk::from_path(&path)?;
    let path_to_world = path
        .as_ref()
        .parent()
        .expect("expected path to have parent");
    let output_dir = PathBuf::new()
        .join(path_to_world)
        .join(format!("{world_name}_scene"));

    match fs::exists(&output_dir) {
        Ok(false) => fs::create_dir(output_dir)
            .unwrap_or_else(|e| panic!("could not create output dir: {e}")),
        Ok(true) => {}
        Err(e) => panic!("could not determine if output dir exists: {e}"),
    }

    let mut tileset_map = HashMap::default();
    map.defs.tilesets.iter().for_each(|t| {
        let src_image_path = PathBuf::new()
            .join(path_to_world)
            .join(t.rel_path.as_ref().expect("no source image for tileset"));

        let mut components = src_image_path.components();
        components.next();

        tileset_map.insert(
            t.uid,
            (
                image::open(&src_image_path).unwrap(),
                components.as_path().to_str().unwrap().to_owned(),
                (t.c_wid, t.c_hei),
            ),
        );
    });

    let mut entity_map = HashMap::default();
    map.defs.entities.iter().for_each(|e| {
        entity_map.insert(e.uid, e.clone());
    });

    let mut levels = Vec::with_capacity(map.levels.len());

    for level in map.levels.iter() {
        println!("parsing level: {}", level.identifier);

        let mut composite = RgbaImage::new(level.px_wid as u32, level.px_hei as u32);
        let mut composite_layers = 0;
        let mut new_level = Level::default();

        let mut entities = Vec::new();

        if let Some(layers) = &level.layer_instances {
            for (z, layer) in layers.iter().rev().enumerate() {
                println!("\tparsing layer: {}", layer.identifier);

                if layer.layer_instance_type == "Entities" {
                    for entity in layer.entity_instances.iter() {
                        entities.push(LdtkEntity {
                            ident: LdtkEntityIdent(entity.identifier.clone()),
                            px: Vec2::new(entity.px[0] as f32, entity.px[1] as f32),
                        });
                    }

                    continue;
                }

                if let Some(tileset_uid) = layer.tileset_def_uid {
                    let (src_img, src_img_path, (width, height)) = tileset_map
                        .get(&tileset_uid)
                        .expect("no image registered for tileset");
                    let tile_size = layer.grid_size as u32;

                    let grid_tiles = match &*layer.layer_instance_type {
                        "IntGrid" => &layer.auto_layer_tiles,
                        "Tiles" => &layer.grid_tiles,
                        t => {
                            println!("unknown layer instance type [{t}], skipping");
                            continue;
                        }
                    };

                    if layer.identifier.to_lowercase().contains("background") {
                        composite_layers += 1;
                        for tile in grid_tiles.iter() {
                            let (px_x, px_y) = (tile.px[0] as i32, tile.px[1] as i32);
                            let (src_x, src_y) = (tile.src[0] as i32, tile.src[1] as i32);

                            let x_range: Vec<_> = if tile.f & 1 == 1 {
                                (0..tile_size as i32).rev().collect()
                            } else {
                                (0..tile_size as i32).collect()
                            };

                            let y_range: Vec<_> = if tile.f & 2 == 2 {
                                (0..tile_size as i32).rev().collect()
                            } else {
                                (0..tile_size as i32).collect()
                            };

                            for (iy, dy) in y_range.iter().enumerate() {
                                for (ix, dx) in x_range.iter().enumerate() {
                                    let dst = composite.get_pixel_mut(
                                        px_x as u32 + ix as u32,
                                        px_y as u32 + iy as u32,
                                    );
                                    let src = src_img
                                        .get_pixel((src_x + *dx) as u32, (src_y + *dy) as u32);

                                    dst.0 = blend_rgba(dst.0, src.0);
                                }
                            }
                        }
                    } else {
                        new_level.tilesets.push(TileSet {
                            asset_path: src_img_path.clone(),
                            // top_left: Vec2::new(0., 0.),
                            tile_size: layer.grid_size as u32,
                            width: *width as u32,
                            height: *height as u32,
                            tiles: grid_tiles
                                .iter()
                                .map(|t| Tile {
                                    xy: Vec2::new(t.px[0] as f32, t.px[1] as f32),
                                    index: t.t as u32,
                                    flip_x: t.f & 1 == 1,
                                    flip_y: t.f & 2 == 2,
                                })
                                .collect(),
                            z: z as f32,
                        });
                    }
                } else {
                    println!("\t\tno tileset, skipping");
                }
            }
        }

        if composite_layers > 0 {
            let path = PathBuf::new()
                .join(path_to_world)
                .join(format!("{world_name}_scene"))
                .join(format!("{world_name}_background_composite.png"));
            let mut file = File::create(&path).unwrap_or_else(|e| {
                panic!(
                    "Error creating {world_name} {} background composite file: {e}",
                    level.identifier
                )
            });
            composite.write_to(&mut file, ImageFormat::Png).unwrap();

            let mut components = path.components();
            components.next();
            new_level.background_asset_path =
                Some(components.as_path().to_str().unwrap().to_owned());
        }

        levels.push((new_level, entities));
    }

    for (level, entities) in levels.into_iter() {
        save_level(level, entities, path_to_world, world_name);
    }

    Ok(())
}

pub fn entities<P: AsRef<Path>>(path: P) -> Result<String, Box<dyn Error>> {
    let mut input = proc_macro2::TokenStream::new();

    input.append_all(quote! {
        use bevy::prelude::*;

        pub struct LdtkEntityPlugin;

        impl Plugin for LdtkEntityPlugin {
            fn build(&self, app: &mut App) {
                app.add_systems(PreUpdate, init_spawned_entities);
            }
        }

        trait LdtkEntity: Component {}
    });

    let mut entity_ident_map = Vec::new();
    let mut names = Vec::new();
    for entry in walkdir::WalkDir::new(&path) {
        let entry = entry.unwrap();
        let path = entry.path();

        if path.extension().is_some_and(|e| e == "ldtk") {
            let map = Ldtk::from_path(path)?;

            for entity in map.defs.entities.iter() {
                let ldtk_name =
                    Ident::new(&format!("Ldtk{}", entity.identifier), Span::call_site());
                names.push(ldtk_name.clone());
                let ldtk_identifier = &entity.identifier;

                input.append_all(quote! {
                    #[derive(Debug, Clone, Copy, Default, Component)]
                    pub struct #ldtk_name;
                    impl LdtkEntity for #ldtk_name {}
                });

                entity_ident_map.push(
                    quote! { #ldtk_identifier => Some(LdtkEntities::#ldtk_name(#ldtk_name)), },
                );
            }
        }
    }

    input.append_all(quote! {
        #[warn(clippy::enum_variant_names)]
        enum LdtkEntities {
            #(#names(#names)),*
        }
    });

    input.append_all(quote! {
        fn init_spawned_entities(
            mut commands: Commands,
            entities: Query<(Entity, &bevy_ldtk_scene::LdtkEntityIdent), Added<bevy_ldtk_scene::LdtkEntityIdent>>,
        ) {
            for (entity, ident) in entities.iter() {

                if let Some(ldtk_entity) = entity_from_ident(&ident.0) {
                    let mut entity = commands.entity(entity);
                    match ldtk_entity {
                        #(LdtkEntities::#names(e) => entity.insert(e),)*
                    };
                }
            }
        }
    });

    input.append_all(quote! {
        fn entity_from_ident(ident: &str) -> Option<LdtkEntities> {
            match ident {
                #(#entity_ident_map)*
                e => {
                    error!("LDtk entity not binded: {e}");
                    None
                }
            }
        }
    });

    let parsed: syn::File =
        syn::parse2(input).unwrap_or_else(|e| panic!("Error parsing generated code: {e}"));
    let output = prettyplease::unparse(&parsed);

    Ok(output)
}

fn save_level(level: Level, entities: Vec<LdtkEntity>, path_to_world: &Path, world_name: &str) {
    let mut app = App::default();
    register_types(&mut app);
    let world = app.world_mut();

    world.spawn(level);
    for entity in entities.into_iter() {
        world.spawn((
            entity.ident,
            Transform::from_xyz(entity.px.x, entity.px.y, 100.),
        ));
    }

    let scene = DynamicSceneBuilder::from_world(world)
        .deny_resource::<Time<Real>>()
        .extract_entities(world.iter_entities().map(|entity| entity.id()))
        .extract_resources()
        .build();
    let type_registry = world.resource::<AppTypeRegistry>();
    let type_registry = type_registry.read();
    let serialized_scene = scene.serialize(&type_registry).unwrap();

    File::create(
        PathBuf::new()
            .join(path_to_world)
            .join(format!("{world_name}_scene"))
            .join(format!("{world_name}.scn.ron")),
    )
    .and_then(|mut file| file.write(serialized_scene.as_bytes()))
    .unwrap_or_else(|e| panic!("Error while writing {world_name} scene to file: {e}"));
}

fn blend_rgba(color1: [u8; 4], color2: [u8; 4]) -> [u8; 4] {
    let c1 = color1.map(|c| c as f32 / 255.0);
    let c2 = color2.map(|c| c as f32 / 255.0);
    let a_out = c1[3] + c2[3] * (1.0 - c1[3]);

    if a_out == 0.0 {
        return [0, 0, 0, 0];
    }

    let blended_color = [
        (c1[0] * c1[3] * (1.0 - c2[3]) + c2[0] * c2[3]) / a_out,
        (c1[1] * c1[3] * (1.0 - c2[3]) + c2[1] * c2[3]) / a_out,
        (c1[2] * c1[3] * (1.0 - c2[3]) + c2[2] * c2[3]) / a_out,
        a_out,
    ];

    blended_color.map(|c| (c * 255.0).round() as u8)
}

pub(crate) fn register_types(app: &mut App) {
    app.register_type::<Transform>()
        .register_type::<LdtkEntityIdent>()
        .register_type::<LevelIdentifier>()
        .register_type::<Level>()
        .register_type::<TileSet>()
        .register_type::<Tile>();
}
