use std::{
    collections::{BTreeMap, BTreeSet},
    process::id,
};

use darling::{
    ast::{Data, Fields, NestedMeta},
    error::Accumulator,
    FromDeriveInput, FromMeta, FromVariant,
};
use proc_macro2::{Ident, TokenStream};
use quote::format_ident;
use syn::{Meta, Type, Visibility};

use crate::derive::{ExtTrait, State};

use super::StateMachine;

pub(super) fn parse(input: &syn::DeriveInput) -> Result<StateMachine, TokenStream> {
    StateMachineDef::from_derive_input(&input)
        .and_then(TryFrom::try_from)
        .map_err(|err| err.write_errors())
}

/// A definition of a state machine
#[derive(Debug, Clone, FromDeriveInput, PartialEq, Eq)]
#[darling(attributes(state_machine))]
#[darling(supports(enum_newtype))]
struct StateMachineDef {
    ident: Ident,
    vis: Visibility,
    data: Data<StateDef, ()>,
    #[darling(default)]
    ext_trait: ExtTraitDef,
}

impl TryFrom<StateMachineDef> for StateMachine {
    type Error = darling::Error;

    fn try_from(
        StateMachineDef {
            ident,
            vis,
            data,
            ext_trait,
        }: StateMachineDef,
    ) -> Result<Self, Self::Error> {
        let mut errs = Accumulator::default();
        let states: BTreeMap<_, _> = data
            .take_enum()
            .expect("Only enum should be accepted by darling")
            .into_iter()
            .map(
                |StateDef {
                     ident,
                     mut fields,
                     to,
                     try_to,
                 }| {
                    (ident.clone(), {
                        let to: BTreeSet<_> = to
                            .into_iter()
                            .flatten()
                            .chain(Some(ident.clone())) // identity transform is always ok
                            .collect();
                        let try_to = try_to
                            .into_iter()
                            .flatten()
                            .inspect(|tt| {
                                if to.contains(tt) {
                                    errs.push(
                                        darling::Error::custom(
                                            "Transition already present as infallible",
                                        )
                                        .with_span(tt),
                                    )
                                }
                            })
                            .collect();
                        State {
                            name: ident,
                            content: fields.fields.pop().unwrap(),
                            to,
                            try_to,
                        }
                    })
                },
            )
            .collect();
        for State { to, try_to, .. } in states.values() {
            for dest in to.iter().chain(try_to) {
                if !states.contains_key(dest) {
                    errs.push(darling::Error::custom("Expected enum variant").with_span(dest))
                }
            }
        }
        errs.finish_with(Self {
            ext_trait: ExtTrait {
                name: ext_trait.name.unwrap_or_else(|| format_ident!("{ident}SM")),
                vis: ext_trait.vis.unwrap_or(vis),
            },
            name: ident,
            states,
        })
    }
}

/// The trait containing all the state machine functions
#[derive(Debug, Clone, FromMeta, PartialEq, Eq, Default)]
struct ExtTraitDef {
    /// The name of the trait
    #[darling(default)]
    name: Option<Ident>,
    /// Visibility of the trait
    #[darling(default)]
    vis: Option<Visibility>,
}

/// A state of the machine
#[derive(Debug, Clone, FromVariant, PartialEq, Eq)]
#[darling(supports(newtype))]
#[darling(attributes(state))]
struct StateDef {
    ident: Ident,
    fields: Fields<Type>,
    #[darling(multiple)]
    to: Vec<ToDef>,
    #[darling(multiple)]
    try_to: Vec<ToDef>,
}

/// A state of the machine
#[derive(Debug, Clone, PartialEq, Eq)]
struct ToDef(BTreeSet<Ident>);

impl IntoIterator for ToDef {
    type Item = Ident;

    type IntoIter = <BTreeSet<Ident> as IntoIterator>::IntoIter;

    fn into_iter(self) -> <BTreeSet<Ident> as IntoIterator>::IntoIter {
        self.0.into_iter()
    }
}
impl FromMeta for ToDef {
    fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
        let mut idents = BTreeSet::new();
        let mut errs = Accumulator::default();
        for item in items {
            if let NestedMeta::Meta(Meta::Path(path)) = item {
                if let Some(ident) = path.get_ident() {
                    idents.insert(ident.clone());
                    continue;
                }
            }
            errs.push(darling::Error::custom("Expected variant name").with_span(&item))
        }
        errs.finish_with(Self(idents))
    }
}

#[cfg(test)]
mod tests {
    mod from_derive_input_to_def {
        use darling::FromDeriveInput;
        use syn::{parse_quote, Visibility};

        use crate::derive::parse::StateMachineDef;

        #[test]
        fn empty() {
            let input = parse_quote!(
                enum Sm {
                    A(A),
                }
            );
            let StateMachineDef {
                ident,
                vis,
                data,
                ext_trait,
            } = StateMachineDef::from_derive_input(&input).unwrap();
            assert_eq!(ident, "Sm");
            assert_eq!(vis, Visibility::Inherited);
            assert_eq!(ext_trait, Default::default());
            assert!(data.is_enum());
            let [a] = &data.take_enum().unwrap()[..] else {
                panic!()
            };
            assert_eq!(a.ident, "A");
            assert!(a.fields.is_newtype());
            assert_eq!(a.fields.fields[0], parse_quote!(A));
            assert!(a.to.is_empty());
            assert!(a.try_to.is_empty());
        }

        #[test]
        fn to() {
            let input = parse_quote!(
                enum Sm {
                    #[state(to(A, B))]
                    A(A),
                    #[state(to(A), to(A, B))]
                    B(B),
                    #[state(to(A))]
                    #[state(to(B))]
                    C(C),
                }
            );
            let StateMachineDef { data, .. } = StateMachineDef::from_derive_input(&input).unwrap();
            let [a, _, _] = &data.take_enum().unwrap()[..] else {
                panic!()
            };
            assert_eq!(a.to.len(), 1);
            assert_eq!(a.to[0].0.len(), 2);
        }
        #[test]
        fn try_to() {
            let input = parse_quote!(
                enum Sm {
                    #[state(try_to(A, B))]
                    A(A),
                    #[state(try_to(A), try_to(A, B))]
                    B(B),
                    #[state(try_to(A))]
                    #[state(try_to(B))]
                    C(C),
                }
            );
            let StateMachineDef { data, .. } = StateMachineDef::from_derive_input(&input).unwrap();
            let [a, _, _] = &data.take_enum().unwrap()[..] else {
                panic!()
            };
            assert_eq!(a.try_to.len(), 1);
            assert_eq!(a.try_to[0].0.len(), 2);
        }

        #[test]
        fn ext_trait() {
            let input = parse_quote!(
                #[state_machine(ext_trait)]
                enum Sm {}
            );
            let StateMachineDef { data, .. } = StateMachineDef::from_derive_input(&input).unwrap();
            let [a, _, _] = &data.take_enum().unwrap()[..] else {
                panic!()
            };
            assert_eq!(a.to.len(), 1);
            assert_eq!(a.to[0].0.len(), 2);
        }
    }
}
