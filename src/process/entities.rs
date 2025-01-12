use crate::{
    asset_loader::{EntityInstances, HotLdtkWorld},
    extract::entities::{
        Atlas, DynLdtkEntity, EntityInstanceUid, EntityUid, ExtractEntityFields, ExtractEntityTypes,
    },
    world::LevelUid,
    HotWorld,
};
use bevy::{ecs::system::SystemId, prelude::*, sprite::Anchor, utils::hashbrown::HashMap};

#[derive(Default, Resource)]
pub struct LevelEntityRegistry {
    pub entities: HashMap<LevelUid, SystemId>,
}

/// Root entity of a set of [`LevelEntity`]s.
#[derive(Debug, Clone, Copy, Component)]
#[require(Transform, Visibility)]
pub struct LevelEntities(pub LevelUid);

#[derive(Component)]
pub struct LevelEntity;

pub fn spawn_entities(
    mut commands: Commands,
    entity_query: Query<(Entity, &LevelEntities), Without<Children>>,
    registry: Res<LevelEntityRegistry>,
) {
    for (_, level_entities) in entity_query.iter() {
        // println!("hi");
        // if let Some(id) = registry.entities.get(&level_entities.0) {
        //     println!("hello");
        //     commands.run_system(*id);
        // }
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

#[allow(clippy::too_many_arguments)]
pub(crate) fn process_entities(
    mut commands: Commands,
    server: Res<AssetServer>,
    entity_query: Query<(Entity, &DynLevelEntities), Without<Children>>,
    entities: Res<Assets<EntityInstances>>,
    mut layouts: ResMut<Assets<TextureAtlasLayout>>,
    registry: Res<LevelDynEntityRegistry>,
    world: Option<Single<&HotWorld>>,
    worlds: Res<Assets<HotLdtkWorld>>,
) {
    let Some(world) = world else {
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
                    let new_entity = commands.spawn_empty().id();
                    component.insert(fields, &mut commands.entity(new_entity));
                    new_entity
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
                .insert(Transform::from_translation(ldtk_entity.xyz))
                .insert(LevelEntity);

            commands.entity(entity).add_child(new_entity);
        }
    }
}

pub fn update_entities(
    mut commands: Commands,
    mut reader: EventReader<AssetEvent<EntityInstances>>,
    handle_query: Query<(Entity, &DynLevelEntities)>,
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
