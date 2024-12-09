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
    id: String,
    tilesets: Vec<TileSet>,
    background_asset_path: Option<String>,
}

#[derive(Debug, Reflect)]
struct TileSet {
    asset_path: String,
    tile_size: u32,
    width: u32,
    height: u32,
    z: f32,
    tiles: Vec<Tile>,
}

#[derive(Debug, Reflect)]
struct Tile {
    xy: Vec2,
    index: u32,
    flip_x: bool,
    flip_y: bool,
}

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
                    info!("loading tileset: {}", tileset.asset_path);
                    let layout = atlases.add(TextureAtlasLayout::from_grid(
                        UVec2::splat(tileset.tile_size),
                        tileset.width,
                        tileset.height,
                        None,
                        None,
                    ));
                    let image = asset_server.load(&tileset.asset_path);

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
                                tile.xy.x, -tile.xy.y, tileset.z,
                            )),
                        ));
                    }
                }
            });

        commands.entity(entity).despawn();
    }
}

pub struct LdtkEntity {
    ident: LdtkEntityIdent,
    px: Vec2,
}

#[derive(Debug, Default, Reflect, Component)]
#[reflect(Component)]
pub struct LdtkEntityIdent(pub String);
