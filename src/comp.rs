use crate::*;
use convert_case::{Case, Casing};
use foldhash::HashMap;
use image::{DynamicImage, GenericImageView, ImageFormat, RgbaImage};
use ldtk2::{serde_json::Value, Ldtk, TileInstance};
use quote::{format_ident, quote, TokenStreamExt};
use std::{
    fs::File,
    io::Write,
    path::{Path, PathBuf},
};

/// Perform an action on every LDtk file in a given path.
pub fn walk_ldtk_dir<F, O, P: AsRef<Path>>(path: P, f: F) -> Result<Vec<O>, ldtk2::Error>
where
    F: Fn(&Path) -> Result<O, ldtk2::Error>,
{
    let mut output = Vec::new();
    for entry in walkdir::WalkDir::new(path) {
        let entry = entry.unwrap();
        let path = entry.path();

        if path.extension().is_some_and(|e| e == "ldtk") {
            let res = f(path)?;
            output.push(res);
        }
    }

    Ok(output)
}

#[derive(Debug, Clone)]
pub struct WorldModule {
    pub world_name: String,
    pub data: String,
}

/// Constructs a mod heiarchy:
///     World -> Entities/Enums | Levels -> [`spawn` and `despawn`]
///
/// Generates a pair of functions within each level mod to `spawn` and `despawn`.
pub fn build_ldtk_world_mod(path: &Path) -> Result<WorldModule, ldtk2::Error> {
    let mut output = proc_macro2::TokenStream::new();
    let world = LdtkWorld::new(path)?;

    output.append_all(quote! { use bevy::prelude::*; });
    output.append_all(world.extract_enums());
    output.append_all(world.extract_entities());

    for level in world.extract_levels() {
        let level_name = format_ident!("{}", level.name.to_case(Case::Snake));
        let level_name_pascal = format_ident!("{}", level.name.to_case(Case::Pascal));
        let uid = level.uid.0;
        let entity_data = level.entities.into_iter().map(|e| e.data);

        let path_to_tile_data = PathBuf::new()
            .join(&world.path)
            .join(format!("{}.scn.ron", uid));
        File::create(&path_to_tile_data)
            .and_then(|mut file| file.write(level.tile_ron_data.as_bytes()))
            .unwrap_or_else(|e| panic!("Error while writing tild data to file: {e}"));
        let path_to_tile_data = path_to_tile_data
            .strip_prefix("assets/")
            .unwrap()
            .to_owned()
            .into_os_string()
            .into_string()
            .unwrap();

        let layouts = level.texture_layouts;
        output.append_all(quote! {
            pub mod #level_name {
                use bevy::prelude::*;
                pub const UID: bevy_ldtk_scene::LevelUid = bevy_ldtk_scene::LevelUid(#uid);
                #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect, Component)]
                #[reflect(Component)]
                pub struct #level_name_pascal;

                pub fn spawn(
                    mut commands: Commands,
                    asset_server: Res<AssetServer>,
                    mut atlases: ResMut<Assets<TextureAtlasLayout>>
                ) {
                    commands.run_system_cached(super::register_tileset_enums);
                    commands.spawn(
                        (
                            DynamicSceneRoot(asset_server.load(#path_to_tile_data)),
                            #level_name_pascal,
                            UID,
                            Transform::default(),
                            Visibility::Visible
                        )
                    )
                    .with_children(|root| {
                        #layouts
                        #(root.spawn(#entity_data);)*
                    });
                }

                fn despawn(mut commands: Commands, level_query: Single<Entity, With<#level_name_pascal>>) {
                    commands.entity(level_query.into_inner()).despawn_recursive();
                }
            }
        });
    }

    output.append_all(world.extract_tilesets());

    Ok(WorldModule {
        world_name: world.name,
        data: tokens_to_string(output),
    })
}

#[derive(Debug)]
pub struct LdtkWorld {
    name: String,
    path: String,
    ldtk: Ldtk,
    tile_set_defs: TileSetDefs,
    enum_registry: EnumRegistry,
}

impl LdtkWorld {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self, ldtk2::Error> {
        let name = path
            .as_ref()
            .file_stem()
            .expect("invalid path")
            .to_str()
            .expect("invalid unicode")
            .to_owned();
        let ldtk = Ldtk::from_path(&path)?;
        let path = path
            .as_ref()
            .parent()
            .expect("expected path to have parent");
        let tile_set_defs = TileSetDefs::new(path, &ldtk);
        let enum_registry = EnumRegistry::new(&name, &ldtk.defs.enums);

        Ok(Self {
            path: path
                .to_owned()
                .into_os_string()
                .into_string()
                .expect("invalid ldtk file path"),
            name,
            ldtk,
            tile_set_defs,
            enum_registry,
        })
    }

    pub fn levels(&self) -> impl Iterator<Item = &ldtk2::Level> {
        self.ldtk.levels.iter()
    }

    pub fn layers(
        &self,
    ) -> impl Iterator<Item = (&ldtk2::Level, impl Iterator<Item = &ldtk2::LayerInstance>)> {
        self.levels()
            .filter_map(|l| l.layer_instances.as_ref().map(|layers| (l, layers.iter())))
    }

    pub fn tiles(
        &self,
    ) -> impl Iterator<
        Item = (
            &ldtk2::Level,
            &ldtk2::LayerInstance,
            impl Iterator<Item = &ldtk2::TileInstance>,
        ),
    > {
        self.layers()
            .map(|(level, layers)| {
                layers.filter_map(move |l| match &*l.layer_instance_type {
                    "IntGrid" => Some((level, l, l.auto_layer_tiles.iter())),
                    "Tiles" => Some((level, l, l.grid_tiles.iter())),
                    _ => None,
                })
            })
            .flatten()
    }

    pub fn entities(
        &self,
    ) -> impl Iterator<
        Item = (
            &ldtk2::Level,
            &ldtk2::LayerInstance,
            impl Iterator<Item = &ldtk2::EntityInstance>,
        ),
    > {
        self.layers()
            .map(|(level, layers)| {
                layers.filter_map(move |l| match &*l.layer_instance_type {
                    "Entities" => Some((level, l, l.entity_instances.iter())),
                    _ => None,
                })
            })
            .flatten()
    }

    pub fn tile_set_def(&self, uid: &TileSetUid) -> Option<&TileSetDef> {
        self.tile_set_defs.map.get(uid)
    }

    pub fn extract_composites(&self) -> Vec<(String, RgbaImage)> {
        let mut composites = Vec::new();

        for (level, layers) in self.layers().map(|(level, layers)| {
            (
                level,
                layers.filter(|l| l.identifier.to_lowercase().contains("background")),
            )
        }) {
            let mut composite = RgbaImage::new(level.px_wid as u32, level.px_hei as u32);
            let mut writes = 0;

            let layers = layers.collect::<Vec<_>>();
            for layer in layers.iter().rev() {
                let tile_size = layer.grid_size;
                let (uid, tiles) = layer.tiles();
                if let Some(tileset) = uid.and_then(|uid| self.tile_set_def(&uid)) {
                    if let Some(image) = &tileset.image {
                        for tile in tiles {
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
                                    let src =
                                        image.get_pixel((src_x + *dx) as u32, (src_y + *dy) as u32);

                                    dst.0 = blend_rgba(dst.0, src.0);
                                    writes += 1;
                                }
                            }
                        }
                    }
                }
            }

            if writes > 0 {
                composites.push((level.identifier.clone(), composite));
            }
        }

        composites
    }

    pub fn extract_entities(&self) -> proc_macro2::TokenStream {
        let mut entity_defs = HashMap::default();
        let mut output = proc_macro2::TokenStream::new();
        for entity in self.ldtk.defs.entities.iter() {
            entity_defs.insert(entity.uid, ());

            let name = format_ident!("{}", entity.identifier.to_case(Case::Pascal));
            let fields = entity
                .field_defs
                .iter()
                .filter_map(|f| parse_entity_field_type(f, &self.enum_registry))
                .collect::<Vec<_>>();

            if fields.is_empty() {
                output.append_all(quote! {
                    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect, Component)]
                    #[reflect(Component)]
                    pub struct #name;
                });
            } else {
                output.append_all(quote! {
                    #[derive(Debug, Clone, PartialEq, Reflect, Component)]
                    #[reflect(Component)]
                    pub struct #name {
                        #(#fields,)*
                    }
                });
            }
        }

        output
    }

    pub fn extract_enums(&self) -> proc_macro2::TokenStream {
        let mut output = proc_macro2::TokenStream::new();

        let decls = self.enum_registry.declarations();
        output.append_all(quote! {
            #(#decls)*
        });

        output
    }

    pub fn extract_tilesets(&self) -> proc_macro2::TokenStream {
        let mut output = proc_macro2::TokenStream::new();
        let mut register = proc_macro2::TokenStream::new();

        for (tileset_uid, def) in self.tile_set_defs.map.iter() {
            if let Some(enum_uid) = def.def.tags_source_enum_uid {
                let enum_ty = self.enum_registry.type_path_from_uid(&EnumUid(enum_uid));

                for enum_value in def.def.enum_tags.iter() {
                    let len = enum_value.tile_ids.len();
                    let ids = enum_value.tile_ids.iter().map(|i| *i as u32);
                    match enum_ty {
                        EnumType::Marker(markers) => {
                            let marker = markers
                                .get(&enum_value.enum_value_id)
                                .expect("invalid variant");
                            let name = format_ident!(
                                "{}_{}",
                                &enum_value.enum_value_id,
                                tileset_uid.0.to_string()
                            );

                            output.append_all(quote! {
                                #[allow(non_upper_case_globals)]
                                pub const #name: [u32; #len] = [
                                    #(#ids,)*
                                ];
                            });

                            let uid = tileset_uid.0;
                            register.append_all(quote! {
                                registry.0.insert(bevy_ldtk_scene::comp::TileSetUid(#uid), (Box::new(#marker), &#name));
                            });
                        }
                        _ => unimplemented!(),
                    }
                }
            }
        }

        output.append_all(quote! {
            fn register_tileset_enums(mut registry: ResMut<bevy_ldtk_scene::LevelTileEnums>, mut registered: Local<bool>) {
                if !*registered {
                    *registered = true;

                    #register
                }
            }
        });

        output
    }

    pub fn extract_levels(&self) -> Vec<ExtractedLevel> {
        let mut extracted_levels = HashMap::default();
        let mut levels = HashMap::default();

        for (level_identifier, composite) in self.extract_composites() {
            let path = PathBuf::new()
                .join(&self.path)
                .join(format!("{}_background_composite.png", &self.name));
            let mut file = File::create(&path).unwrap_or_else(|e| {
                panic!(
                    "Error creating {} {} background composite file: {e}",
                    &self.name, &level_identifier
                )
            });
            composite.write_to(&mut file, ImageFormat::Png).unwrap();
            let level = levels
                .entry(level_identifier.clone())
                .or_insert_with(|| Level::default());
            level.background_asset_path = Some(
                path.strip_prefix("assets/")
                    .expect("LDtk file is not in the assets folder")
                    .to_owned()
                    .into_os_string()
                    .into_string()
                    .unwrap(),
            );
        }

        for (level, layer, tiles) in self
            .tiles()
            .filter(|(_, l, _)| !l.identifier.to_lowercase().contains("background"))
        {
            let extracted_level = levels
                .entry(level.identifier.clone())
                .or_insert_with(|| Level::default());

            if let Some(mut tileset) = layer.tileset_def_uid.and_then(|uid| {
                self.tile_set_def(&TileSetUid(uid))
                    .map(|t| t.new_tile_set())
            }) {
                tileset.tiles = tiles.map(|t| Tile::from_ldtk_tile(t)).collect();
                extracted_level.tilesets.push(tileset);
            }
        }

        for (level_identifier, level) in levels.into_iter() {
            let extracted_level = extracted_levels
                .entry(level_identifier)
                .or_insert_with(|| ExtractedLevel::from_uid(level.uid.0));
            extracted_level.tile_ron_data = save_tile_ron_data(level);
        }

        for (level, _, entities) in self.entities() {
            let extracted_level = extracted_levels
                .entry(level.identifier.clone())
                .or_insert_with(|| ExtractedLevel::from_uid(level.uid));
            extracted_level.name = level.identifier.clone();

            let mut layouts = HashMap::default();
            for entity in entities {
                let (x, y) = (entity.px[0] as f32, entity.px[1] as f32);
                let rect = match entity.tags.iter().all(|t| t != "atlas").then_some({
                    entity.tile.as_ref().map(|t| {
                        Rect::new(
                            t.x as f32,
                            t.y as f32,
                            (t.x + t.w) as f32,
                            (t.y + t.h) as f32,
                        )
                    })
                }) {
                    Some(r) => match r {
                        Some(r) => {
                            let x0 = r.min.x;
                            let y0 = r.min.y;
                            let x1 = r.max.x;
                            let y1 = r.max.y;
                            quote! { Some(Rect::new(#x0, #y0, #x1, #y1)) }
                        }
                        None => {
                            quote! { None }
                        }
                    },
                    None => quote! { None },
                };

                let tileset = entity.tile.as_ref().map(|t| {
                    self.tile_set_def(&TileSetUid(t.tileset_uid))
                        .expect("entity tileset is not registered")
                        .new_tile_set()
                });

                let sprite = tileset
                    .as_ref()
                    .map(|tileset| {
                        tileset
                            .ty
                            .expect_tiles()
                            .map(|path| {
                                let tile_size = tileset.tile_size;
                                let width = tileset.width;
                                let height = tileset.height;
                                let spacing = tileset.spacing;
                                let _padding = tileset.padding;

                                let len = layouts.len();
                                let (layout, _) =
                            layouts
                                .entry((tile_size, width, height))
                                .or_insert_with(|| {
                                    let ident = format_ident!("layout_{}", len);
                                    (
                                        ident.clone(),
                                        quote! {
                                            let #ident = atlases.add(TextureAtlasLayout::from_grid(
                                                UVec2::splat(#tile_size),
                                                #width,
                                                #height,
                                                Some(UVec2::splat(#spacing)),
                                                None,
                                                // Some(UVec2::splat(#padding)),
                                            ));
                                        },
                                    )
                                });

                                quote! {
                                    Sprite {
                                        rect: #rect,
                                        image: asset_server.load(#path),
                                        anchor: bevy::sprite::Anchor::TopLeft,
                                        texture_atlas: Some(TextureAtlas {
                                            index: 0,
                                            layout: #layout.clone(),
                                        }),
                                        ..Default::default()
                                    }
                                }
                            })
                            .unwrap_or_default()
                    })
                    .unwrap_or_default();

                let entity_name = format_ident!("{}", entity.identifier.to_case(Case::Pascal));
                let fields = entity
                    .field_instances
                    .iter()
                    .filter_map(|f| parse_entity_field_value(f, &self.enum_registry))
                    .collect::<Vec<_>>();
                let entity = if fields.is_empty() {
                    quote! { super::#entity_name::default() }
                } else {
                    quote! {
                        super::#entity_name {
                            #(#fields,)*
                        }
                    }
                };
                let layouts = layouts.values().map(|(_, layout)| layout);
                extracted_level.texture_layouts = quote! { #(#layouts)* };
                extracted_level.entities.push(ExtractedEntityInstance {
                    data: quote! {
                        (
                            #entity,
                            Transform::from_translation(Vec2::new(#x, -#y).extend(0.)),
                            Visibility::Visible,
                            #sprite
                        )
                    },
                });
            }
        }

        extracted_levels.into_values().collect()
    }
}

fn save_tile_ron_data(level: Level) -> String {
    let mut app = App::default();
    register_types(&mut app);
    let world = app.world_mut();

    world.spawn(level);
    let scene = DynamicSceneBuilder::from_world(world)
        .deny_resource::<Time<Real>>()
        .extract_entities(world.iter_entities().map(|entity| entity.id()))
        .extract_resources()
        .build();
    let type_registry = world.resource::<AppTypeRegistry>();
    let type_registry = type_registry.read();
    scene.serialize(&type_registry).unwrap()
}

#[derive(Debug, Default)]
pub struct ExtractedLevel {
    uid: LevelUid,
    name: String,
    tile_ron_data: String,
    entities: Vec<ExtractedEntityInstance>,
    texture_layouts: proc_macro2::TokenStream,
}

impl ExtractedLevel {
    pub fn from_uid(uid: i64) -> Self {
        Self {
            uid: LevelUid(uid),
            ..Default::default()
        }
    }
}

#[derive(Debug)]
pub struct ExtractedEntityInstance {
    data: proc_macro2::TokenStream,
}

fn tokens_to_string(tokens: proc_macro2::TokenStream) -> String {
    let parsed: syn::File =
        syn::parse2(tokens).unwrap_or_else(|e| panic!("Error parsing generated code: {e}"));
    prettyplease::unparse(&parsed)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumUid(pub i64);

/// Stores the absolute path declaration of an enum.
#[derive(Debug)]
struct EnumRegistry {
    ty_path_from_uid: HashMap<EnumUid, EnumType>,
    ty_path: HashMap<String, EnumType>,
    decls: Vec<proc_macro2::TokenStream>,
}

impl EnumRegistry {
    pub fn new(world_name: &str, enums: &[ldtk2::EnumDefinition]) -> Self {
        let mut ty_path_from_uid = HashMap::default();
        let mut ty_path = HashMap::default();
        let mut decls = Vec::new();

        for enumeration in enums.iter() {
            let world_name = format_ident!("{}", world_name.to_case(Case::Snake));
            let name = format_ident!("{}", &enumeration.identifier.to_case(Case::Pascal));
            let variants = enumeration
                .values
                .iter()
                .map(|v| format_ident!("{}", v.id.to_case(Case::Pascal)))
                .collect::<Vec<_>>();

            if enumeration.tags.iter().any(|t| t.contains("marker")) {
                let mut marker_variants = HashMap::default();
                let ids = enumeration.values.iter().map(|v| &v.id).collect::<Vec<_>>();
                for (variant, id) in variants.iter().zip(ids.iter()) {
                    let name = format_ident!(
                        "{}{}",
                        &enumeration.identifier.to_case(Case::Pascal),
                        variant
                    );
                    marker_variants.insert((**id).clone(), quote! { crate:: #world_name :: #name });
                    decls.push(quote! {
                        #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect, Component)]
                        #[reflect(Component)]
                        pub struct #name;
                    });
                }

                ty_path.insert(
                    enumeration.identifier.clone(),
                    EnumType::Marker(marker_variants.clone()),
                );
                ty_path_from_uid
                    .insert(EnumUid(enumeration.uid), EnumType::Marker(marker_variants));
            } else {
                ty_path.insert(
                    enumeration.identifier.clone(),
                    EnumType::Enum(quote! { crate:: #world_name :: #name }),
                );
                ty_path_from_uid.insert(
                    EnumUid(enumeration.uid),
                    EnumType::Enum(quote! { crate:: #world_name :: #name }),
                );
                decls.push(quote! {
                    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect, Component)]
                    #[reflect(Component)]
                    pub enum #name {
                        #(#variants,)*
                    }
                });
            }
        }

        Self {
            ty_path_from_uid,
            ty_path,
            decls,
        }
    }

    pub fn type_path(&self, key: &str) -> &EnumType {
        self.ty_path.get(key).expect("unregistered enum")
    }

    pub fn type_path_from_uid(&self, key: &EnumUid) -> &EnumType {
        self.ty_path_from_uid.get(key).expect("unregistered enum")
    }

    pub fn declarations(&self) -> impl Iterator<Item = &proc_macro2::TokenStream> {
        self.decls.iter()
    }
}

#[derive(Debug)]
enum EnumType {
    /// The key is the enum variant
    Marker(HashMap<String, proc_macro2::TokenStream>),
    Enum(proc_macro2::TokenStream),
}

fn parse_entity_field_type(
    field: &ldtk2::FieldDefinition,
    enum_registry: &EnumRegistry,
) -> Option<proc_macro2::TokenStream> {
    if field.field_definition_type.contains("LocalEnum") {
        let name = format_ident!("{}", field.identifier.to_case(Case::Snake));
        match enum_registry.type_path(
            &field
                .field_definition_type
                .strip_prefix("LocalEnum.")
                .unwrap(),
        ) {
            EnumType::Enum(ty) => Some(quote! { pub #name: #ty }),
            EnumType::Marker(_) => panic!("an entity cannot have a marker enum"),
        }
    } else {
        unimplemented!()
    }
}

fn parse_entity_field_value(
    field: &ldtk2::FieldInstance,
    enum_registry: &EnumRegistry,
) -> Option<proc_macro2::TokenStream> {
    if field.field_instance_type.contains("LocalEnum") {
        let name = format_ident!("{}", field.identifier.to_case(Case::Snake));
        match enum_registry.type_path(
            &field
                .field_instance_type
                .strip_prefix("LocalEnum.")
                .unwrap(),
        ) {
            EnumType::Enum(ty) => {
                let value = field
                    .value
                    .as_ref()
                    .expect("expected field instance to have a value");
                let variant = match value {
                    Value::String(str) => {
                        format_ident!("{}", str.to_case(Case::Pascal))
                    }
                    _ => {
                        panic!("expected string value for enum");
                    }
                };

                Some(quote! { #name: #ty :: #variant })
            }
            EnumType::Marker(_) => panic!("an entity cannot have a marker enum"),
        }
    } else {
        None
    }
}

pub trait LayerTiles {
    fn tiles(&self) -> (Option<TileSetUid>, impl Iterator<Item = &TileInstance>);
}

impl LayerTiles for ldtk2::LayerInstance {
    fn tiles(&self) -> (Option<TileSetUid>, impl Iterator<Item = &TileInstance>) {
        (
            self.tileset_def_uid.map(TileSetUid),
            match &*self.layer_instance_type {
                "IntGrid" => self.auto_layer_tiles.iter(),
                "Tiles" => self.grid_tiles.iter(),
                // Should be emtpy
                _ => self.grid_tiles.iter(),
            },
        )
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub struct TileSetUid(pub i64);

#[derive(Debug)]
pub struct TileSetDefs {
    map: HashMap<TileSetUid, TileSetDef>,
}

impl TileSetDefs {
    fn new(path_to_world: &Path, ldtk: &Ldtk) -> Self {
        let mut map = HashMap::default();

        ldtk.defs.tilesets.iter().for_each(|t| {
            map.insert(TileSetUid(t.uid), TileSetDef::new(path_to_world, t));
        });

        Self { map }
    }

    pub fn get(&self, uid: &TileSetUid) -> Option<&TileSetDef> {
        self.map.get(uid)
    }
}

#[derive(Debug)]
pub struct TileSetDef {
    def: ldtk2::TilesetDefinition,
    image: Option<DynamicImage>,
    image_path: Option<String>,
}

impl TileSetDef {
    pub fn new(path_to_world: &Path, def: &ldtk2::TilesetDefinition) -> Self {
        println!("{:#?}", def.enum_tags);

        let image = def.rel_path.as_ref().and_then(|p| {
            let path = PathBuf::new().join(path_to_world).join(&p);
            match image::open(&path) {
                Ok(image) => Some(image),
                Err(e) => {
                    warn!("failed to open tileset def source image: {e}");
                    None
                }
            }
        });

        let image_path = def.rel_path.as_ref().and_then(|p| {
            let path = PathBuf::new().join(path_to_world).join(&p);
            let mut components = path.components();
            components.next();
            components.as_path().to_str().map(|s| s.to_owned())
        });

        Self {
            def: def.clone(),
            image,
            image_path,
        }
    }

    pub fn new_tile_set(&self) -> TileSet {
        TileSet {
            ty: TileSetType::Tiles(self.image_path.clone()),
            tile_size: self.def.tile_grid_size as u32,
            width: self.def.c_wid as u32,
            height: self.def.c_hei as u32,
            spacing: self.def.spacing as u32,
            padding: self.def.padding as u32,
            uid: TileSetUid(self.def.uid),
            tiles: Vec::new(),
        }
    }
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
    app.register_type::<Level>();
}

//fn entity_field_instance_enum(
//    path: &Path,
//    field: &str,
//    enum_name: Option<&str>,
//) -> Result<String, ldtk2::Error> {
//    let mut output = proc_macro2::TokenStream::new();
//
//    for entry in walkdir::WalkDir::new(&path) {
//        let entry = entry.unwrap();
//        let path = entry.path();
//
//        if path.extension().is_some_and(|e| e == "ldtk") {
//            let world = LdtkWorld::new(path)?;
//
//            for (level, _, entities) in world.entities() {
//                let mut instance_values = Vec::new();
//
//                for entity in entities {
//                    for field_instance in entity.field_instances.iter() {
//                        if field_instance.identifier == field {
//                            if let Some(value) = &field_instance.value {
//                                match value {
//                                    Value::String(str) => {
//                                        let pascal = str.to_case(Case::Pascal);
//                                        let ident = format_ident!("{pascal}");
//                                        instance_values.push(quote! { #ident });
//                                    }
//                                    _ => unimplemented!(),
//                                }
//                            }
//                        }
//                    }
//                }
//
//                let name = enum_name
//                    .map(|name| format_ident!("{}", name))
//                    .unwrap_or_else(|| format_ident!("{}", field.to_case(Case::Pascal)));
//                let level_name = format_ident!("{}", level.identifier.to_case(Case::Snake));
//                output.append_all(quote! {
//                    pub mod #level_name {
//                        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//                        pub enum #name {
//                            #(#instance_values,)*
//                        }
//                    }
//                });
//            }
//        }
//    }
//
//    let parsed: syn::File =
//        syn::parse2(output).unwrap_or_else(|e| panic!("Error parsing generated code: {e}"));
//    let output = prettyplease::unparse(&parsed);
//
//    Ok(output)
//}
//
//fn level_entity_enum(ldtk_entities: &[&ldtk2::EntityInstance]) -> proc_macro2::TokenStream {
//    let mut output = proc_macro2::TokenStream::new();
//
//    let mut visited = Vec::new();
//    let entities = ldtk_entities.iter().filter_map(|e| {
//        if !visited.contains(&e.identifier) {
//            visited.push(e.identifier.clone());
//            Some(format_ident!("{}", e.identifier.to_case(Case::Pascal)))
//        } else {
//            None
//        }
//    });
//
//    let mut visited = Vec::new();
//    let entities_from_ident = ldtk_entities.iter().filter_map(|e| {
//        if !visited.contains(&e.identifier) {
//            visited.push(e.identifier.clone());
//            let variant = format_ident!("{}", e.identifier.to_case(Case::Pascal));
//            let name = &e.identifier;
//            Some(quote! { #name => Some(Self::#variant) })
//        } else {
//            None
//        }
//    });
//
//    let mut visited = Vec::new();
//    let entities_to_ident = ldtk_entities.iter().filter_map(|e| {
//        if !visited.contains(&e.identifier) {
//            visited.push(e.identifier.clone());
//            let variant = format_ident!("{}", e.identifier.to_case(Case::Pascal));
//            let name = &e.identifier;
//            Some(quote! { Self::#variant => #name })
//        } else {
//            None
//        }
//    });
//
//    output.append_all(quote! {
//        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//        pub enum Entities {
//            #(#entities,)*
//        }
//
//        impl Entities {
//            pub fn from_ident(ident: &str) -> Option<Self> {
//                match ident {
//                    #(#entities_from_ident,)*
//                    _ => None
//                }
//            }
//
//            pub fn ident(&self) -> &'static str {
//                match self {
//                    #(#entities_to_ident,)*
//                }
//            }
//        }
//    });
//
//    output
//}
//
//pub trait RegisteredEntityComponent: 'static + Send + Sync {
//    fn insert(&self, entity: &mut EntityCommands<'_>);
//}
//
//fn level_entity_markers(
//    level: &ldtk2::Level,
//    ldtk_entities: &[&ldtk2::EntityInstance],
//) -> proc_macro2::TokenStream {
//    let mut output = proc_macro2::TokenStream::new();
//
//    let mut visited = Vec::new();
//    let entity_names = ldtk_entities.iter().filter_map(|e| {
//        if !visited.contains(&e.identifier) {
//            visited.push(e.identifier.clone());
//            Some(format_ident!("Ldtk{}", e.identifier.to_case(Case::Pascal)))
//        } else {
//            None
//        }
//    });
//
//    let mut visited = Vec::new();
//    let func_names = ldtk_entities
//        .iter()
//        .filter_map(|e| {
//            if !visited.contains(&e.identifier) {
//                visited.push(e.identifier.clone());
//                Some(format_ident!(
//                    "register_{}",
//                    e.identifier.to_case(Case::Snake)
//                ))
//            } else {
//                None
//            }
//        })
//        .collect::<Vec<_>>();
//
//    output.append_all(quote! {
//        pub trait RegisterLdtkEntityComponent {
//            #(fn #func_names <R>(&mut self) -> &mut Self
//            where
//                R: Default + Component;)*
//        }
//        impl RegisterLdtkEntityComponent for App {
//            #(fn #func_names <R>(&mut self) -> &mut Self
//            where
//                R: Default + Component,
//            {
//                self.register_required_components::<#entity_names, R>()
//            })*
//        }
//    });
//
//    let mut visited = Vec::new();
//    for entity in ldtk_entities.iter() {
//        if !visited.contains(&entity.identifier) {
//            visited.push(entity.identifier.clone());
//            let ldtk_name = Ident::new(&format!("Ldtk{}", entity.identifier), Span::call_site());
//
//            let fields = entity
//                .field_instances
//                .iter()
//                .map(|f| {
//                    let name = format_ident!("{}", f.identifier.to_case(Case::Snake));
//                    if f.field_instance_type.contains("Enum") {
//                        println!("[{}]", &f.field_instance_type);
//                        let enum_name = format_ident!(
//                            "{}",
//                            f.field_instance_type
//                                .strip_prefix("LocalEnum.")
//                                .unwrap()
//                                .to_case(Case::Pascal)
//                        );
//                        Some(quote! { #name: super::#enum_name })
//                    } else {
//                        None
//                    }
//
//                    //match f.field_instance_type.as_str() {
//                    //    "Enum" => {
//                    //        quote! { #name: String }
//                    //    }
//                    //    _ => unimplemented!(),
//                    //}
//                })
//                .collect::<Vec<_>>();
//
//            if fields.is_empty() {
//                output.append_all(quote! {
//                    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Component)]
//                    pub struct #ldtk_name;
//                });
//            } else {
//                output.append_all(quote! {
//                    #[derive(Debug, Clone, Default, Component)]
//                    pub struct #ldtk_name {
//                        #(#fields,)*
//                    }
//                });
//            }
//
//            output.append_all(quote! {
//                impl bevy_ldtk_scene::comp::RegisteredEntityComponent for #ldtk_name {
//                    fn insert(&self, entity: &mut EntityCommands<'_>) {
//                        entity.insert(Self::default());
//                    }
//                }
//            });
//        }
//    }
//
//    let level_name = level.identifier.to_case(Case::Pascal);
//    let level_plugin = format_ident!("{}Plugin", &level_name);
//    let level_entity_registry = format_ident!("{}EntityRegistry", &level_name);
//
//    let mut visited = Vec::new();
//    let registry_insertion = ldtk_entities.iter().filter_map(|entity| {
//        if !visited.contains(&entity.identifier) {
//            visited.push(entity.identifier.clone());
//            let ident = &entity.identifier;
//            let ldtk_name = Ident::new(&format!("Ldtk{}", entity.identifier), Span::call_site());
//            Some(quote! { #ident, Box::new(#ldtk_name::default()) })
//        } else {
//            None
//        }
//    });
//
//    output.append_all(quote! {
//        pub struct #level_plugin;
//        impl Plugin for #level_plugin {
//            fn build(&self, app: &mut App) {
//                let mut registry = #level_entity_registry ::default();
//                #(registry.0.insert(#registry_insertion);)*
//                app
//                    .insert_resource(registry)
//                    .add_systems(PreUpdate, init_spawned_entities);
//            }
//        }
//
//        #[derive(Default, Resource)]
//        pub struct #level_entity_registry (bevy::utils::hashbrown::HashMap<&'static str, Box<dyn bevy_ldtk_scene::comp::RegisteredEntityComponent>>);
//
//        fn init_spawned_entities(
//            mut commands: Commands,
//            entities: Query<(Entity, &bevy_ldtk_scene::LdtkEntityIdent), Added<bevy_ldtk_scene::LdtkEntityIdent>>,
//            registry: Res<#level_entity_registry>
//        ) {
//            for (entity, ldtk_entity) in entities.iter() {
//                let mut entity = commands.entity(entity);
//                if let Some(component) = registry.0.get(ldtk_entity.0.as_str()) {
//                    component.insert(&mut entity);
//                } else {
//                    error!("{} is not registered", &ldtk_entity.0);
//                }
//            }
//        }
//    });
//
//    output
//}
