use bevy::prelude::*;
use bevy::utils::hashbrown::HashMap;
use comp::TileSetUid;
use ldtk2::TileInstance;

pub mod comp;

pub struct LdtkScenePlugin;

impl Plugin for LdtkScenePlugin {
    fn build(&self, app: &mut App) {
        comp::register_types(app);
        app.insert_resource(LevelTileEnums::default())
            .add_systems(PreUpdate, spawn_level_tiles);
    }
}

#[derive(Debug, Default, Reflect, Component)]
#[reflect(Component)]
pub struct LevelUid(pub i64);

#[derive(Debug, Default, Reflect, Component)]
#[reflect(Component)]
pub struct Level {
    pub uid: LevelUid,
    pub tilesets: Vec<TileSet>,
    pub background_asset_path: Option<(String, Vec2)>,
}

#[derive(Debug, Reflect)]
pub struct TileSet {
    pub ty: TileSetType,
    pub tile_size: u32,
    pub width: u32,
    pub height: u32,
    pub spacing: u32,
    pub padding: u32,
    pub uid: TileSetUid,
    pub tiles: Vec<Tile>,
    pub z: f32,
}

#[derive(Debug, Reflect)]
pub enum TileSetType {
    /// Asset path
    Tiles(Option<String>),
    IntGrid,
}

impl TileSetType {
    pub fn expect_tiles(&self) -> Option<&String> {
        match self {
            Self::Tiles(tiles) => tiles.as_ref(),
            _ => panic!("expected tiles"),
        }
    }
}

#[derive(Debug, Reflect)]
pub struct Tile {
    pub xy: Vec2,
    pub index: u32,
    pub flip_x: bool,
    pub flip_y: bool,
}

impl Tile {
    pub fn from_ldtk_tile(tile: &TileInstance, offset: Option<Vec2>) -> Self {
        Self {
            xy: Vec2::new(tile.px[0] as f32, tile.px[1] as f32) + offset.unwrap_or_default(),
            index: tile.t as u32,
            flip_x: tile.f & 1 == 1,
            flip_y: tile.f & 2 == 2,
        }
    }
}

pub trait TileEnum: 'static + Send + Sync {
    fn insert(&self, entity: &mut EntityCommands<'_>);
}

impl<T> TileEnum for T
where
    T: Clone + Component,
{
    fn insert(&self, entity: &mut EntityCommands<'_>) {
        entity.insert(self.clone());
    }
}

#[derive(Default, Resource)]
pub struct LevelTileEnums(pub HashMap<TileSetUid, Vec<(Box<dyn TileEnum>, &'static [u32])>>);

fn spawn_level_tiles(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    levels: Query<(Entity, &Level), Added<Level>>,
    mut atlases: ResMut<Assets<TextureAtlasLayout>>,
    registry: Res<LevelTileEnums>,
) {
    for (entity, level) in levels.iter() {
        commands
            .entity(entity)
            .insert((Transform::default(), Visibility::Visible))
            .remove::<Level>()
            .with_children(|parent| {
                if let Some(background) = &level.background_asset_path {
                    parent.spawn((
                        Sprite {
                            image: asset_server.load(&background.0),
                            anchor: bevy::sprite::Anchor::TopLeft,
                            ..Default::default()
                        },
                        Transform::from_translation(background.1.extend(-100.)),
                    ));
                }

                for tileset in level.tilesets.iter() {
                    match &tileset.ty {
                        TileSetType::Tiles(asset_path) => {
                            if let Some(asset_path) = asset_path {
                                let layout = atlases.add(TextureAtlasLayout::from_grid(
                                    UVec2::splat(tileset.tile_size),
                                    tileset.width,
                                    tileset.height,
                                    None,
                                    None,
                                ));
                                let image = asset_server.load(asset_path);

                                let enum_hash = registry.0.get(&tileset.uid).map(|r| {
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
                                for tile in tileset.tiles.iter() {
                                    let mut sprite = Sprite {
                                        image: image.clone(),
                                        texture_atlas: Some(TextureAtlas {
                                            index: tile.index as usize,
                                            layout: layout.clone(),
                                        }),
                                        anchor: bevy::sprite::Anchor::TopLeft,
                                        ..Default::default()
                                    };

                                    sprite.flip_x = tile.flip_x;
                                    sprite.flip_y = tile.flip_y;

                                    let mut entity = parent.spawn((
                                        sprite,
                                        Transform::from_translation(Vec3::new(
                                            tile.xy.x, -tile.xy.y, tileset.z,
                                        )),
                                    ));

                                    if let Some(enums) = &enum_hash {
                                        for (component, index) in enums.iter() {
                                            if index.get(&tile.index).is_some() {
                                                component.insert(&mut entity);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        _ => {
                            unimplemented!()
                        }
                    }
                }
            });
    }
}
