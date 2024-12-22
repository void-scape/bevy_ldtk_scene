use bevy::prelude::*;
use bevy::utils::hashbrown::HashMap;
use ldtk2::TileInstance;

use self::comp::TileSetUid;

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
    pub background_asset_path: Option<String>,
}

#[derive(Debug, Reflect)]
pub struct TileSet {
    pub ty: TileSetType,
    pub tile_size: u32,
    pub width: u32,
    pub height: u32,
    pub uid: TileSetUid,
    pub tiles: Vec<Tile>,
}

#[derive(Debug, Reflect)]
pub enum TileSetType {
    /// Asset path
    Tiles(Option<String>),
    IntGrid,
}

#[derive(Debug, Reflect)]
pub struct Tile {
    pub xy: Vec2,
    pub index: u32,
    pub flip_x: bool,
    pub flip_y: bool,
}

impl Tile {
    pub fn from_ldtk_tile(tile: &TileInstance) -> Self {
        Self {
            xy: Vec2::new(tile.px[0] as f32, tile.px[1] as f32),
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
pub struct LevelTileEnums(HashMap<TileSetUid, (Box<dyn TileEnum>, &'static [usize])>);

fn spawn_level_tiles(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    levels: Query<(Entity, &Level), Added<Level>>,
    mut atlases: ResMut<Assets<TextureAtlasLayout>>,
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
                            image: asset_server.load(background),
                            anchor: bevy::sprite::Anchor::TopLeft,
                            ..Default::default()
                        },
                        Transform::from_xyz(0., 0., -100.),
                    ));
                }

                for (z, tileset) in level.tilesets.iter().rev().enumerate() {
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

                                    parent.spawn((
                                        sprite,
                                        Transform::from_translation(Vec3::new(
                                            tile.xy.x, -tile.xy.y, z as f32,
                                        )),
                                    ));
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
