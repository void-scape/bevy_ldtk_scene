use bevy::{asset::StrongHandle, prelude::*, tasks::IoTaskPool};
use foldhash::HashMap;
use image::{GenericImageView, ImageFormat, RgbaImage};
use ldtk2::{Ldtk, TileInstance};
use std::{
    borrow::Cow,
    error::Error,
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    time::Duration,
};

pub fn build_ldtk_scene<P: AsRef<Path>>(path: P) -> Result<(), Box<dyn Error>> {
    // let map = Ldtk::from_path(&path)?;
    // let asset_folder_path = "../annual/assets/ldtk/";
    //
    // // let map = Ldtk::from_path("assets/test.ldtk")?;
    // // println!("{map:#?}");
    // // let asset_folder_path = "assets/";
    //
    // let mut tileset_map = HashMap::default();
    // map.defs.tilesets.iter().for_each(|t| {
    //     tileset_map.insert(
    //         t.uid,
    //         image::open(
    //             PathBuf::new()
    //                 .join(asset_folder_path)
    //                 .join(t.rel_path.as_ref().expect("no source image for tileset")),
    //         )
    //         .unwrap(),
    //     );
    // });
    //
    // let mut entity_map = HashMap::default();
    // map.defs.entities.iter().for_each(|e| {
    //     entity_map.insert(e.uid, e.clone());
    // });
    //
    // for level in map.levels.iter() {
    //     println!("parsing level: {}", level.identifier);
    //     let mut composite = RgbaImage::new(level.px_wid as u32, level.px_hei as u32);
    //
    //     if let Some(layers) = &level.layer_instances {
    //         for layer in layers
    //             .iter()
    //             .rev()
    //             .filter(|l| l.identifier.to_lowercase().contains("background"))
    //         {
    //             println!("\tparsing layer: {}", layer.identifier);
    //
    //             if let Some(tileset_uid) = layer.tileset_def_uid {
    //                 let src_img = tileset_map
    //                     .get(&tileset_uid)
    //                     .expect("no image registered for tileset");
    //                 let tile_size = layer.grid_size as u32;
    //
    //                 let grid_tiles = match &*layer.layer_instance_type {
    //                     "IntGrid" => &layer.auto_layer_tiles,
    //                     "Tiles" => &layer.grid_tiles,
    //                     t => {
    //                         println!("unknown layer instance type [{t}], skipping");
    //                         continue;
    //                     }
    //                 };
    //
    //                 for tile in grid_tiles.iter() {
    //                     let (px_x, px_y) = (tile.px[0] as i32, tile.px[1] as i32);
    //                     let (src_x, src_y) = (tile.src[0] as i32, tile.src[1] as i32);
    //
    //                     let x_range: Vec<_> = if tile.f & 1 == 1 {
    //                         (0..tile_size as i32).rev().collect()
    //                     } else {
    //                         (0..tile_size as i32).collect()
    //                     };
    //
    //                     let y_range: Vec<_> = if tile.f & 2 == 2 {
    //                         (0..tile_size as i32).rev().collect()
    //                     } else {
    //                         (0..tile_size as i32).collect()
    //                     };
    //
    //                     for (iy, dy) in y_range.iter().enumerate() {
    //                         for (ix, dx) in x_range.iter().enumerate() {
    //                             let dst = composite.get_pixel_mut(
    //                                 px_x as u32 + ix as u32,
    //                                 px_y as u32 + iy as u32,
    //                             );
    //                             let src =
    //                                 src_img.get_pixel((src_x + *dx) as u32, (src_y + *dy) as u32);
    //
    //                             dst.0 = blend_rgba(dst.0, src.0);
    //                         }
    //                     }
    //                 }
    //             } else {
    //                 println!("\t\tno tileset, skipping");
    //             }
    //         }
    //     }
    //
    //     let file = format!("assets/ldtk/{}_background_composite", level.identifier);
    //     let fout = &mut File::create(Path::new(&format!("{file}.png"))).unwrap();
    //     composite.write_to(fout, ImageFormat::Png).unwrap();
    // }

    let mut app = App::default();
    app.register_type::<TileSet>()
        .register_type::<Tile>()
        .register_type_data::<&'static str, ReflectSerialize>();
    app.world_mut().spawn(TileSet {
        src: "hello!",
        tiles: vec![Tile { idx: 69 }],
    });
    save_scene(app.world_mut());

    Ok(())
}

#[derive(Reflect, Component)]
#[reflect(Component)]
struct TileSet {
    src: &'static str,
    tiles: Vec<Tile>,
}

#[derive(Reflect)]
struct Tile {
    idx: u32,
}

const NEW_SCENE_FILE_PATH: &str = "scenes/load_scene_example-new.scn.ron";

fn save_scene(world: &mut World) {
    let scene = DynamicSceneBuilder::from_world(world)
        .deny_resource::<Time<Real>>()
        .extract_entities(world.iter_entities().map(|entity| entity.id()))
        .extract_resources()
        .build();
    let type_registry = world.resource::<AppTypeRegistry>();
    let type_registry = type_registry.read();
    let serialized_scene = scene.serialize(&type_registry).unwrap();

    File::create(format!("assets/{NEW_SCENE_FILE_PATH}"))
        .and_then(|mut file| file.write(serialized_scene.as_bytes()))
        .expect("Error while writing scene to file");
}

fn blend_rgba(color1: [u8; 4], color2: [u8; 4]) -> [u8; 4] {
    // Extract RGBA components as floats in the range [0.0, 1.0]
    let c1 = color1.map(|c| c as f32 / 255.0);
    let c2 = color2.map(|c| c as f32 / 255.0);

    // Alpha out: A_out = A1 + A2 * (1 - A1)
    let a_out = c1[3] + c2[3] * (1.0 - c1[3]);

    // If there's no alpha, return fully transparent
    if a_out == 0.0 {
        return [0, 0, 0, 0];
    }

    // Blend each channel: C_out = (C1 * A1 * (1 - A2) + C2 * A2) / A_out
    let blended_color = [
        (c1[0] * c1[3] * (1.0 - c2[3]) + c2[0] * c2[3]) / a_out,
        (c1[1] * c1[3] * (1.0 - c2[3]) + c2[1] * c2[3]) / a_out,
        (c1[2] * c1[3] * (1.0 - c2[3]) + c2[2] * c2[3]) / a_out,
        a_out, // Alpha is calculated directly
    ];

    // Convert back to u8 range [0, 255]
    blended_color.map(|c| (c * 255.0).round() as u8)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build() {
        build_ldtk_scene("assets/test.ldtk").unwrap();
    }
}
