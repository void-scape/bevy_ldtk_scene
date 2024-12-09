use bevy::prelude::*;

pub mod comp;

pub struct LdtkScenePlugin;

impl Plugin for LdtkScenePlugin {
    fn build(&self, app: &mut App) {
        comp::register_types(app);
        app.add_systems(PreUpdate, populate_levels);
    }
}

#[derive(Debug, Reflect, Component)]
#[reflect(Component)]
#[require(Transform, Visibility)]
pub struct LevelIdentifier(pub String);

#[derive(Debug, Default, Reflect, Component)]
#[reflect(Component)]
struct Level {
    pub id: String,
    pub tilesets: Vec<TileSet>,
    pub background_asset_path: Option<String>,
}

#[derive(Debug, Reflect)]
struct TileSet {
    pub desc: TileSetDesc,
    pub tiles: Vec<Tile>,
}

#[derive(Debug, Reflect, Component)]
#[reflect(Component)]
pub struct TileSetDesc {
    pub asset_path: String,
    pub tile_size: u32,
    pub width: u32,
    pub height: u32,
    pub z: f32,
}

#[derive(Debug, Reflect)]
struct Tile {
    pub xy: Vec2,
    pub index: u32,
    pub flip_x: bool,
    pub flip_y: bool,
}

#[derive(Debug, Default, Reflect, Component)]
pub struct LdtkEntity {
    pub ident: LdtkEntityIdent,
    pub tileset: Option<TileSetDesc>,
    pub px: Vec2,
}

#[derive(Debug, Default, Reflect, Component)]
#[reflect(Component)]
pub struct LdtkEntityIdent(pub String);

fn populate_levels(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    levels: Query<(Entity, &Level), Added<Level>>,
    mut atlases: ResMut<Assets<TextureAtlasLayout>>,
) {
    for (entity, level) in levels.iter() {
        commands
            .spawn(LevelIdentifier(level.id.clone()))
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

                for tileset in level.tilesets.iter() {
                    let layout = atlases.add(TextureAtlasLayout::from_grid(
                        UVec2::splat(tileset.desc.tile_size),
                        tileset.desc.width,
                        tileset.desc.height,
                        None,
                        None,
                    ));
                    let image = asset_server.load(&tileset.desc.asset_path);

                    for tile in tileset.tiles.iter() {
                        let mut sprite = Sprite::from_atlas_image(
                            image.clone(),
                            TextureAtlas {
                                index: tile.index as usize,
                                layout: layout.clone(),
                            },
                        );

                        sprite.flip_x = tile.flip_x;
                        sprite.flip_y = tile.flip_y;

                        parent.spawn((
                            sprite,
                            Transform::from_translation(Vec3::new(
                                tile.xy.x,
                                -tile.xy.y,
                                tileset.desc.z,
                            )),
                        ));
                    }
                }
            });

        commands.entity(entity).despawn();
    }
}
