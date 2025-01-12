use crate::*;
use convert_case::{Case, Casing};
use extract::world::{ExtractedComponent, FromLdtkWorld, IntoExtractedComponent};
use foldhash::HashMap;
use quote::{format_ident, quote, TokenStreamExt};
use world::ExtractLdtkWorld;

/// Extract enums defined in the `Project Enums` tab.
pub struct ExtractEnums;

impl IntoExtractedComponent<ExtractedEnums> for ExtractEnums {
    type Context = EnumRegistry;

    fn extract(self, _: &ExtractLdtkWorld, context: Self::Context) -> ExtractedEnums {
        let decls = context.declarations();
        ExtractedEnums {
            decls: quote! {
                #(#decls)*
            },
            register_types: context
                .definitions()
                .flat_map(|d| match d {
                    EnumDefinition::Enum { ty, .. } => {
                        vec![ty.clone()]
                    }
                    EnumDefinition::Marker(variants) => variants.values().cloned().collect(),
                })
                .collect(),
        }
    }
}

/// Extracted enums defined in the `Project Enums` tab.
pub struct ExtractedEnums {
    decls: proc_macro2::TokenStream,
    register_types: Vec<syn::Ident>,
}

impl ExtractedComponent for ExtractedEnums {
    fn global(&self, output: &mut proc_macro2::TokenStream) {
        output.append_all(self.decls.clone());
    }

    fn plugin(&self, output: &mut proc_macro2::TokenStream) {
        let types = &self.register_types;
        output.append_all(quote! {
            #(app.register_type::<#types>();)*
        });
    }
}

/// Use [`EnumRegistry::definition_from_uid`] to retrieve the enum definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumUid(pub i64);

/// Maps variant name to definition (e.g. Enum::Value => map.get("Value"))
#[derive(Debug, Clone)]
pub enum EnumDefinition {
    Marker(HashMap<String, syn::Ident>),
    Enum {
        ty: syn::Ident,
        variants: HashMap<String, proc_macro2::TokenStream>,
    },
}

impl EnumDefinition {
    /// Returns the enum's `variant` (e.g. Enum::Variant).
    pub fn variant(&self, variant: &str) -> Option<proc_macro2::TokenStream> {
        match self {
            Self::Marker(map) => map.get(variant).map(|v| quote! { #v }),
            Self::Enum { variants, .. } => variants.get(variant).cloned(),
        }
    }
}

/// Stores enum declarations and definitions.
#[derive(Debug)]
pub struct EnumRegistry {
    def_from_uid: HashMap<EnumUid, EnumDefinition>,
    def: HashMap<String, EnumDefinition>,
    decls: Vec<proc_macro2::TokenStream>,
}

impl EnumRegistry {
    pub fn new(enums: &[ldtk2::EnumDefinition]) -> Self {
        let mut def_from_uid = HashMap::default();
        let mut def = HashMap::default();
        let mut decls = Vec::new();

        for enumeration in enums.iter() {
            let enum_ident = format_ident!("{}", &enumeration.identifier.to_case(Case::Pascal));
            let variants = enumeration
                .values
                .iter()
                .map(|v| format_ident!("{}", v.id.to_case(Case::Pascal)))
                .collect::<Vec<_>>();

            if enumeration.tags.iter().any(|t| t.contains("marker")) {
                let mut marker_variants = HashMap::default();
                let ids = enumeration.values.iter().map(|v| &v.id).collect::<Vec<_>>();
                for (variant, id) in variants.iter().zip(ids.iter()) {
                    let name = format_ident!(
                        "{}{}",
                        &enumeration.identifier.to_case(Case::Pascal),
                        variant
                    );
                    marker_variants.insert((**id).clone(), name.clone());

                    let enum_mod = format_ident!("{}", name.to_string().to_case(Case::Snake));
                    decls.push(quote! {
                        mod #enum_mod {
                            use ::bevy::prelude::ReflectComponent;
                            #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
                            #[derive(::bevy::ecs::component::Component, ::bevy::reflect::Reflect)]
                            #[derive(::serde::Serialize, ::serde::Deserialize)]
                            #[reflect(Component)]
                            pub struct #name;
                        }
                        #[allow(unused)]
                        pub use #enum_mod :: #name;
                    });
                }

                let enum_def = EnumDefinition::Marker(marker_variants);
                def.insert(enumeration.identifier.clone(), enum_def.clone());
                def_from_uid.insert(EnumUid(enumeration.uid), enum_def);
            } else {
                let mut marker_variants = HashMap::default();
                let ids = enumeration.values.iter().map(|v| &v.id).collect::<Vec<_>>();
                for (variant, id) in variants.iter().zip(ids.iter()) {
                    marker_variants.insert((**id).clone(), quote! { #enum_ident :: #variant });
                }

                let enum_mod = format_ident!("{}", enum_ident.to_string().to_case(Case::Snake));
                decls.push(quote! {
                    mod #enum_mod {
                        use ::bevy::prelude::ReflectComponent;
                        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
                        #[derive(::bevy::ecs::component::Component, ::bevy::reflect::Reflect)]
                        #[derive(::serde::Serialize, ::serde::Deserialize)]
                        #[reflect(Component)]
                        pub enum #enum_ident {
                            #(#variants,)*
                        }
                    }
                    #[allow(unused)]
                    pub use #enum_mod :: #enum_ident;
                });

                let enum_def = EnumDefinition::Enum {
                    variants: marker_variants.clone(),
                    ty: enum_ident,
                };
                def.insert(enumeration.identifier.clone(), enum_def.clone());
                def_from_uid.insert(EnumUid(enumeration.uid), enum_def);
            };
        }

        Self {
            def,
            def_from_uid,
            decls,
        }
    }

    pub fn definition(&self, name: &str) -> &EnumDefinition {
        self.def.get(name).expect("unregistered enum")
    }

    pub fn definitions(&self) -> impl Iterator<Item = &EnumDefinition> {
        self.def.values()
    }

    pub fn definition_from_uid(&self, key: &EnumUid) -> &EnumDefinition {
        self.def_from_uid.get(key).expect("unregistered enum")
    }

    pub fn declarations(&self) -> impl Iterator<Item = &proc_macro2::TokenStream> {
        self.decls.iter()
    }
}

impl FromLdtkWorld for EnumRegistry {
    fn from_world(world: &ExtractLdtkWorld) -> Self {
        Self::new(&world.ldtk().defs.enums)
    }
}
