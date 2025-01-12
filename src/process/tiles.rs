use crate::{
    asset_loader::LevelTileSetInstances, extract::tiles::TileSetComponentRegistry, world::LevelUid,
};
use bevy::{prelude::*, utils::hashbrown::HashMap};

/// Root entity of a set of [`TileSetEntity`]s.
#[derive(Component)]
#[require(LevelUid)]
pub struct LevelTileSets(pub Handle<LevelTileSetInstances>);

#[derive(Component)]
pub struct TileSetEntity;

pub fn process_tilesets(
    mut commands: Commands,
    server: Res<AssetServer>,
    tilesets: Res<Assets<LevelTileSetInstances>>,
    mut atlases: ResMut<Assets<TextureAtlasLayout>>,
    component_registry: Res<TileSetComponentRegistry>,
    handle_query: Query<(Entity, &LevelTileSets), Without<Children>>,
) {
    for (entity, handle) in handle_query.iter() {
        let Some(instance) = tilesets.get(&handle.0) else {
            continue;
        };

        commands
            .entity(entity)
            .insert((TileSetEntity, Transform::default(), Visibility::Visible))
            .with_children(|parent| {
                for instance in instance.0.iter() {
                    let tileset = &instance.tileset;

                    let layout = atlases.add(TextureAtlasLayout::from_grid(
                        UVec2::splat(tileset.tile_size),
                        tileset.width,
                        tileset.height,
                        None,
                        None,
                    ));
                    let image = server.load(&tileset.asset_image);

                    let component = component_registry.0.get(&tileset.uid).map(|r| {
                        r.iter()
                            .map(|r| {
                                (
                                    &r.0,
                                    r.1.iter()
                                        .map(|idx| (*idx, ()))
                                        .collect::<HashMap<u32, ()>>(),
                                )
                            })
                            .collect::<Vec<_>>()
                    });

                    // TODO: autotile layers will place mutliple tiles on top of eachother,
                    // which means we need some way to order the front from the back.
                    // This method does not really leave much room for z ordering...
                    let z_delta = 1. / (instance.tiles.len() as f32 * 2.);
                    let mut z = instance.z;
                    for tile in instance.tiles.iter() {
                        let mut entity = parent.spawn(Transform::from_translation(Vec3::new(
                            tile.xy.x, -tile.xy.y, z,
                        )));

                        if let Some(meta) = &tile.sprite {
                            let mut sprite = Sprite {
                                image: image.clone(),
                                texture_atlas: Some(TextureAtlas {
                                    index: tile.index as usize,
                                    layout: layout.clone(),
                                }),
                                anchor: bevy::sprite::Anchor::TopLeft,
                                ..Default::default()
                            };

                            sprite.flip_x = meta.flip_x;
                            sprite.flip_y = meta.flip_y;

                            entity.insert(sprite);
                        }

                        if let Some(component) = &component {
                            for (component, index) in component.iter() {
                                if index.get(&tile.index).is_some() {
                                    component.insert(&mut entity);
                                }
                            }
                        }

                        z += z_delta;
                    }
                }
            });
    }
}

pub fn update_tilesets(
    mut commands: Commands,
    mut reader: EventReader<AssetEvent<LevelTileSetInstances>>,
    handle_query: Query<(Entity, &LevelTileSets)>,
) {
    for event in reader.read() {
        if let AssetEvent::Modified { id } = event {
            for entity in handle_query
                .iter()
                .filter_map(|(entity, handle)| (handle.0.id() == *id).then_some(entity))
            {
                commands
                    .entity(entity)
                    .despawn_descendants()
                    .clear_children();
            }
        }
    }
}
