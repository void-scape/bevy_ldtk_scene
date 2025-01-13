use super::Process;
use bevy::prelude::*;
use quote::{quote, ToTokens, TokenStreamExt};

#[derive(Component)]

pub struct Composite(pub String);

pub struct CompositeProcessor;

impl Process for CompositeProcessor {
    type Component = Composite;
    type Param = Res<'static, AssetServer>;

    fn process(
        component: &Self::Component,
        entity: &mut bevy::prelude::EntityCommands,
        server: &mut <Self::Param as bevy::ecs::system::SystemParam>::Item<'_, '_>,
    ) {
        entity.insert(Sprite {
            image: server.load(&component.0),
            anchor: bevy::sprite::Anchor::TopLeft,
            ..Default::default()
        });
    }
}

impl ToTokens for CompositeProcessor {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.append_all(quote! { ::bevy_ldtk_scene::process::composites::CompositeProcessor });
    }
}
