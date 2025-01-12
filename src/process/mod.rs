use bevy::{
    ecs::system::{StaticSystemParam, SystemParam},
    prelude::*,
};
use quote::{quote, ToTokens, TokenStreamExt};

pub mod composites;
pub mod entities;
pub mod tiles;

/// Process an entity with component `C` when [`Added`] to the ecs.
pub trait Process: 'static {
    type Component: Component;
    type Param: SystemParam + 'static;

    fn process(
        component: &Self::Component,
        entity: &mut EntityCommands,
        param: &mut <Self::Param as SystemParam>::Item<'_, '_>,
    );
}

pub trait ProcessAppExt {
    fn register_processor<P: Process>(&mut self) -> &mut Self;
}

impl ProcessAppExt for App {
    fn register_processor<P: Process>(&mut self) -> &mut Self {
        self.add_systems(PreUpdate, process_entities::<P>.in_set(ProcessSet))
    }
}

/// Use in [`ExtractedComponent::plugin`] to register a [`Process`] in the target.
pub fn register_processor_comp<P>(processor: P, output: &mut proc_macro2::TokenStream)
where
    P: Process + ToTokens,
{
    output.append_all(quote! {
        ::bevy_ldtk_scene::process::ProcessAppExt::register_processor::<#processor>(app);
    });
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SystemSet)]
pub struct ProcessSet;

fn process_entities<P: Process>(
    mut commands: Commands,
    entities: Query<(Entity, &<P as Process>::Component), Added<<P as Process>::Component>>,
    mut param: StaticSystemParam<<P as Process>::Param>,
) {
    for (entity, component) in entities.iter() {
        P::process(component, &mut commands.entity(entity), &mut param);
    }
}
