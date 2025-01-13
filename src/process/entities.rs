use crate::{
    asset_loader::{EntityInstances, HotLdtkWorld},
    extract::entities::{Atlas, DynLdtkEntity, EntityUid, ExtractEntityFields},
    levels::Level,
    world::LevelUid,
    HotWorld,
};
use bevy::{
    ecs::system::SystemId,
    prelude::*,
    sprite::Anchor,
    utils::hashbrown::{HashMap, HashSet},
};

/// Root entity of a set of [`LevelEntity`]s.
#[derive(Debug, Clone, Copy, Component)]
#[require(Transform, Visibility)]
pub struct LevelEntities(pub LevelUid);

/// Spawned as a child of a [`LevelEntities`].
#[derive(Component)]
pub struct LevelEntity;

/// Spawned as a child of the [`World`].
#[derive(Component)]
pub struct WorldlyEntity;

#[derive(Debug, Default, Resource)]
pub struct LevelEntityRegistry {
    pub entities: HashMap<LevelUid, SystemId<In<EntitySystemInput>>>,
}

#[derive(Debug)]
pub struct EntitySystemInput {
    level: Entity,
    world: Entity,
}

impl EntitySystemInput {
    pub fn new(level: Entity, world: Entity) -> Self {
        Self { level, world }
    }

    pub fn level(&self) -> Entity {
        self.level
    }

    pub fn world(&self) -> Entity {
        self.world
    }
}

pub fn spawn_entities(
    mut commands: Commands,
    entity_query: Query<(Entity, &LevelEntities, &Parent), Without<Populated>>,
    levels_query: Query<&Parent, With<Level>>,
    registry: Res<LevelEntityRegistry>,
) {
    for (entity, level_entities, level_entity) in entity_query.iter() {
        if let Some(id) = registry.entities.get(&level_entities.0) {
            if let Ok(world_entity) = levels_query.get(level_entity.get()) {
                commands
                    .run_system_with_input(*id, EntitySystemInput::new(entity, world_entity.get()));
            }
        }

        commands.entity(entity).insert(Populated);
    }
}

/// Root entity of a set of dynamic [`LevelEntity`]s.
#[derive(Component)]
#[require(Transform, Visibility)]
pub struct DynLevelEntities(pub Handle<EntityInstances>);

#[derive(Default, Resource)]
pub struct LevelDynEntityRegistry {
    pub entities: HashMap<EntityUid, Box<dyn DynLdtkEntity>>,
}

#[derive(Debug, Default, Component)]
pub struct WorldlyEntities(pub HashSet<EntityUid>);

#[derive(Component)]
pub struct Populated;

#[allow(clippy::too_many_arguments)]
pub(crate) fn process_dyn_entities(
    mut commands: Commands,
    server: Res<AssetServer>,
    entity_query: Query<(Entity, &DynLevelEntities), Without<Populated>>,
    entities: Res<Assets<EntityInstances>>,
    mut layouts: ResMut<Assets<TextureAtlasLayout>>,
    registry: Res<LevelDynEntityRegistry>,
    world: Option<Single<(&HotWorld, &mut WorldlyEntities)>>,
    worlds: Res<Assets<HotLdtkWorld>>,
) {
    let Some((world, mut worldly)) = world.map(|w| w.into_inner()) else {
        return;
    };

    let Some(world) = worlds.get(&world.0) else {
        return;
    };

    let fields = world.0.extract_component(ExtractEntityFields);

    let mut atlas_registry = HashMap::<(UVec2, Vec<URect>), Handle<TextureAtlasLayout>>::default();
    for (entity, dyn_entities) in entity_query.iter() {
        let Some(entities) = entities.get(&dyn_entities.0) else {
            continue;
        };

        for ldtk_entity in entities.0.iter() {
            let new_entity = if let Some(component) = registry.entities.get(&ldtk_entity.uid) {
                if let Some(fields) = fields.0.get(&ldtk_entity.uid) {
                    if ldtk_entity.worldly && !worldly.0.insert(ldtk_entity.uid) {
                        continue;
                    } else {
                        let new_entity = if ldtk_entity.worldly {
                            commands.spawn(WorldlyEntity).id()
                        } else {
                            commands.spawn(LevelEntity).id()
                        };

                        component.insert(fields, &mut commands.entity(new_entity));
                        new_entity
                    }
                } else {
                    error!("Failed to retreive entity fields: {:?}", ldtk_entity.uid);
                    continue;
                }
            } else {
                error!(
                    "Entity type is unknown: {:?}\n \
                    This plugin cannot load newly created entity types until after rebuilding!",
                    ldtk_entity.uid
                );
                continue;
            };

            if let Some(entity_sprite) = &ldtk_entity.sprite {
                let mut sprite = Sprite {
                    image: server.load(&entity_sprite.image),
                    anchor: Anchor::TopLeft,
                    ..Default::default()
                };

                match &entity_sprite.atlas {
                    Atlas::Rect(rect) => {
                        sprite.rect = Some(*rect);
                    }
                    Atlas::Layout(layout) => {
                        let handle = atlas_registry
                            .entry((layout.size, layout.textures.clone()))
                            .or_insert_with(|| layouts.add(layout.clone()));
                        sprite.texture_atlas = Some(TextureAtlas {
                            index: 0,
                            layout: handle.clone(),
                        });
                    }
                }

                commands.entity(new_entity).insert(sprite);
            }

            commands
                .entity(new_entity)
                .insert(Transform::from_translation(ldtk_entity.xyz));

            commands
                .entity(entity)
                .insert(Populated)
                .add_child(new_entity);
        }
    }
}

pub fn update_dyn_entities(
    mut commands: Commands,
    mut reader: EventReader<AssetEvent<EntityInstances>>,
    handle_query: Query<(Entity, &Children, &DynLevelEntities)>,
    entities: Query<Entity, (With<LevelEntity>, Without<WorldlyEntity>)>,
) {
    for event in reader.read() {
        if let AssetEvent::Modified { id } = event {
            for (entity, children) in
                handle_query
                    .iter()
                    .filter_map(|(entity, children, handle)| {
                        (handle.0.id() == *id).then_some((entity, children))
                    })
            {
                commands.entity(entity).remove::<Populated>();
                for child in children.iter() {
                    if entities.get(*child).is_ok() {
                        commands.entity(*child).despawn();
                    }
                }
            }
        }
    }
}
