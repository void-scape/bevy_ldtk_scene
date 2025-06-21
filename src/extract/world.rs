use crate::*;
use convert_case::{Case, Casing};
use process::{register_processor_comp, Process};
use quote::{format_ident, quote, ToTokens};
use std::{
    ops::Deref,
    path::{Path, PathBuf},
};
use world::{ExtractLdtkWorld, IOResult, WorldIO};

pub struct WorldPlugin(proc_macro2::TokenStream);

impl WorldPlugin {
    pub fn from_extraction(extraction: &impl ExtractedComponent) -> Self {
        let mut output = proc_macro2::TokenStream::new();
        extraction.plugin(&mut output);
        Self(output)
    }

    pub fn processor<P: Process + ToTokens>(mut self, processor: P) -> Self {
        register_processor_comp(processor, &mut self.0);
        self
    }

    pub fn build(self, world: &ExtractLdtkWorld) -> proc_macro2::TokenStream {
        let plugin_name = format_ident!(
            "{}Plugin",
            WorldName::from_world(world).0.to_case(Case::Pascal)
        );
        let plugin = self.0;

        quote! {
            pub struct #plugin_name;

            impl ::bevy::app::Plugin for #plugin_name {
                fn build(&self, app: &mut ::bevy::app::App) {
                    #plugin
                }
            }
        }
    }
}

pub trait FromLdtkWorld {
    fn from_world(world: &ExtractLdtkWorld) -> Self;
}

impl FromLdtkWorld for () {
    fn from_world(_world: &ExtractLdtkWorld) -> Self {}
}

macro_rules! impl_from_world {
    ($($param:ident),*) => {
        impl<$($param),*> FromLdtkWorld for ($($param),*)
        where
            $($param: FromLdtkWorld),*
        {
            fn from_world(world: &ExtractLdtkWorld) -> Self {
                ($($param::from_world(world)),*)
            }
        }
    };
}

variadics_please::all_tuples!(impl_from_world, 2, 15, A);

#[allow(unused)]
pub trait ExtractedComponent {
    fn register_types(&self, registry: &mut AppTypeRegistry) {}

    fn io(&self, io: &mut WorldIO) -> IOResult {
        Ok(())
    }

    fn global(&self, output: &mut proc_macro2::TokenStream) {}

    fn plugin(&self, output: &mut proc_macro2::TokenStream) {}
}

macro_rules! impl_extracted_component {
    ($($param:ident),*) => {
        #[allow(non_snake_case)]
        impl<$($param),*> ExtractedComponent for ($($param),*)
        where
            $($param: ExtractedComponent),*
        {
            fn io(&self, io: &mut WorldIO) -> IOResult {
                let ($($param,)*) = self;
                $($param.io(io)?;)*
                Ok(())
            }

            fn global(&self, output: &mut proc_macro2::TokenStream) {
                let ($($param,)*) = self;
                $($param.global(output));*
            }

            fn plugin(&self, output: &mut proc_macro2::TokenStream) {
                let ($($param,)*) = self;
                $($param.plugin(output));*
            }

            #[allow(non_snake_case)]
            fn register_types(&self, registry: &mut AppTypeRegistry) {
                let ($($param,)*) = self;
                $($param.register_types(registry));*
            }
        }
    };
}

variadics_please::all_tuples!(impl_extracted_component, 2, 15, A);

pub trait IntoExtractedComponent<C> {
    type Context: FromLdtkWorld;

    fn extract(self, world: &ExtractLdtkWorld, context: Self::Context) -> C;
}

macro_rules! impl_into_extracted_component {
    ($(($p:ident, $P:ident)),*) => {
        impl<$($p, $P),*> IntoExtractedComponent<($($p,)*)> for ($($P,)*)
        where
            $($P: IntoExtractedComponent<$p>),*
        {
            type Context = ($($P::Context,)*);

            #[allow(non_snake_case)]
            fn extract(self, world: &ExtractLdtkWorld, ($($P,)*): Self::Context) -> ($($p,)*) {
                let ($($p,)*) = self;
                ($($p.extract(world, $P)),*)
            }
        }
    };
}

variadics_please::all_tuples!(impl_into_extracted_component, 2, 15, A, B);

/// Name of the top level module.
pub struct WorldName(pub String);

impl Deref for WorldName {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl FromLdtkWorld for WorldName {
    fn from_world(world: &ExtractLdtkWorld) -> Self {
        Self(
            world
                .path()
                .file_stem()
                .expect("invalid ldtk file path")
                .to_str()
                .expect("invalid ldtk file ascii name")
                .to_case(Case::Snake),
        )
    }
}

/// Ident of the top level module.
pub struct WorldNameIdent(pub syn::Ident);

impl FromLdtkWorld for WorldNameIdent {
    fn from_world(world: &ExtractLdtkWorld) -> Self {
        Self(format_ident!(
            "{}",
            world
                .path()
                .file_stem()
                .expect("invalid ldtk file path")
                .to_str()
                .expect("invalid ldtk file ascii name")
                .to_case(Case::Snake)
        ))
    }
}

#[derive(Debug, Default, Clone)]
pub struct WorldDirPath(pub PathBuf);

impl FromLdtkWorld for WorldDirPath {
    fn from_world(world: &ExtractLdtkWorld) -> Self {
        Self(
            world
                .path()
                .parent()
                .expect("ldtk file has no parent dir")
                .to_owned(),
        )
    }
}

impl WorldDirPath {
    pub fn asset_path(&self) -> &Path {
        self.0.strip_prefix("assets/").unwrap()
    }
}
