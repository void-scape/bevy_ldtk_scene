use crate::*;
use extract::{
    tiles::{LayerTiles, TileSetRegistry},
    world::{ExtractedComponent, IntoExtractedComponent, WorldDirPath},
};
use image::RgbaImage;
use std::path::PathBuf;
use world::{ExtractLdtkWorld, IOResult, LayerUid, LevelUid, WorldIO};

pub struct ExtractComposites;

impl IntoExtractedComponent<ExtractedComposites> for ExtractComposites {
    type Context = (TileSetRegistry, WorldDirPath);

    fn extract(self, world: &ExtractLdtkWorld, context: Self::Context) -> ExtractedComposites {
        extract_composites(world, context)
    }
}

pub struct ExtractedComposites {
    composites: Vec<Composite>,
}

impl ExtractedComponent for ExtractedComposites {
    fn io(&self, io: &mut WorldIO) -> IOResult {
        for composite in self.composites.iter() {
            io.save_composite(
                (composite.level_uid, composite.layer_uid),
                &format!(
                    "{}_{}_composite.png",
                    &composite.level_ident, &composite.layer_ident
                ),
                &composite.image,
            )?;
        }

        Ok(())
    }
}

struct Composite {
    level_uid: LevelUid,
    layer_uid: LayerUid,
    level_ident: String,
    layer_ident: String,
    image: RgbaImage,
}

pub fn extract_composites(
    world: &ExtractLdtkWorld,
    (tileset_registry, path_to_world): (TileSetRegistry, WorldDirPath),
) -> ExtractedComposites {
    let mut composites = Vec::new();

    for (level, layers) in world.layers().map(|(level, layers)| {
        (
            level,
            layers.filter(|l| l.identifier.to_lowercase().contains("composite")),
        )
    }) {
        let layers = layers.collect::<Vec<_>>();
        for layer in layers.iter().rev() {
            let mut composite = RgbaImage::new(
                (layer.c_wid * layer.grid_size) as u32,
                (layer.c_hei * layer.grid_size) as u32,
            );
            let mut writes = 0;
            let tile_size = layer.grid_size;
            if let Some((uid, tiles)) = layer.tiles() {
                let tileset = tileset_registry.get(uid).unwrap();

                let image = match image::open(
                    PathBuf::new()
                        .join(&path_to_world.0)
                        .join(&tileset.rel_image),
                ) {
                    Ok(image) => image.to_rgba8(),
                    Err(e) => {
                        panic!("failed to open tileset source image: {e}");
                    }
                };

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
                            if let Some(dst) = composite.get_pixel_mut_checked(
                                px_x as u32 + ix as u32,
                                px_y as u32 + iy as u32,
                            ) {
                                if let Some(src) = image
                                    .get_pixel_checked((src_x + *dx) as u32, (src_y + *dy) as u32)
                                {
                                    dst.0 = blend_rgba(dst.0, src.0);
                                    writes += 1;
                                }
                            }
                        }
                    }
                }
            }

            if writes > 0 {
                composites.push(Composite {
                    level_uid: LevelUid(level.uid),
                    layer_uid: LayerUid(layer.layer_def_uid),
                    level_ident: level.identifier.clone(),
                    layer_ident: layer.identifier.clone(),
                    image: composite,
                });
            }
        }
    }

    ExtractedComposites { composites }
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
