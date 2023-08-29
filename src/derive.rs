use std::collections::BTreeMap;

use convert_case::{Case, Casing};
use darling::{
    ast::{Data, Fields, NestedMeta},
    FromDeriveInput, FromMeta, FromMetaItem, FromVariant,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_quote, punctuated::Punctuated, Attribute, DeriveInput, Expr, ExprCall, ExprPath, Ident,
    Meta, MetaList, MetaNameValue, Path, PathSegment, Type, Visibility,
};

// TODO: Setup and emit the MachineStateContent trait
// TODO: Setup and emit the MachineStateResult type
// TODO: Emit the methods

/// Definition of the state machine
#[derive(Debug, FromDeriveInput)]
#[darling(
    supports(enum_newtype),
    attributes(state_machine),
    forward_attrs(state)
)]
struct StateMachineDef {
    /// Name of the state machine
    ident: Ident,
    /// Visibility of the state machine
    vis: Visibility,
    /// Variants of the state machine
    data: Data<StateDef, ()>,
    /// Data for the state enum
    #[darling(default)]
    states_enum: StatesEnumDef,
    /// derive `is_*` methods
    is: Option<bool>,
    /// derive `as_*` methods
    r#as: Option<bool>,
    /// derive `as_*_mut` methods
    as_mut: Option<bool>,
    /// derive `try_into_*` methods
    try_into: Option<bool>,
}

#[derive(Debug, FromMeta, Default)]
struct StatesEnumDef {
    /// Name of the state enum
    name: Option<Ident>,
    /// Visibility of the state enum
    vis: Option<Visibility>,
    /// Attributes of the state enum (if no derive than `derive(Debug, Clone, Copy, Eq, PartialEq, Hash)` is added)
    #[darling(multiple)]
    attrs: Vec<Meta>,
}

#[derive(Debug, FromVariant)]
#[darling(attributes(state))]
struct StateDef {
    /// Name of the variant
    ident: Ident,
    /// Field of the variant
    fields: Fields<Type>,
    /// list of infallible transitions
    #[darling(default)]
    to: DestsDef,
    /// list of fallible transitions
    #[darling(default)]
    try_to: DestsDef,
    /// derive an `is_*` method
    is: Option<bool>,
    /// derive an `as_*` method
    r#as: Option<bool>,
    /// derive an `as_*_mut` method
    as_mut: Option<bool>,
    /// derive an `try_into_*` method
    try_into: Option<bool>,
}

#[derive(Debug, Default)]
struct DestsDef(Vec<DestDef>);
impl FromMeta for DestsDef {
    fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
        Ok(DestsDef(
            items
                .into_iter()
                .map(DestDef::from_nested_meta)
                .try_collect()?,
        ))
    }
}
#[derive(Debug)]
struct DestDef {
    to: Ident,
    fun: Option<ExprPath>,
}
impl FromMeta for DestDef {
    fn from_nested_meta(item: &NestedMeta) -> darling::Result<Self> {
        (match item {
            NestedMeta::Meta(Meta::Path(path)) if let Some(ident) = path.get_ident() => Ok(DestDef{to:ident.clone(), fun:None}),
            NestedMeta::Meta(Meta::NameValue(MetaNameValue{ path, value:Expr::Path(fun),.. })) if let Some(name) = path.get_ident() => Ok(DestDef{to:name.clone(), fun:Some(fun.clone())}),
        _ => Err(darling::Error::custom("Expected state name or `State = path::to::fun`")),
    })
        .map_err(|e| e.with_span(item))
    }
}

struct StateMachine {
    /// Name of the state machine
    ident: Ident,
    /// Variants of the state machine
    states: Vec<State>,
    /// Data for the state enum
    states_enum: StatesEnum,
}

struct StatesEnum {
    /// Name of the state enum
    name: Ident,
    /// Visibility of the state enum
    vis: Visibility,
    /// Attributes of the state enum (if no derive than `derive(Debug, Clone, Copy, Eq, PartialEq, Hash)` is added)
    attrs: Vec<Attribute>,
}
impl StatesEnum {
    fn from_def(
        StatesEnumDef {
            name,
            vis,
            mut attrs,
        }: StatesEnumDef,
        sm_name: &Ident,
        sm_vis: Visibility,
    ) -> StatesEnum {
        // Adding default derives if no derive was specified
        if !attrs
            .iter()
            .any(|attr| matches!(attr, Meta::List(MetaList{ path, ..})if path.is_ident("derive")))
        {
            attrs.push(parse_quote!(derive(
                Debug, Clone, Copy, Eq, PartialEq, Hash
            )))
        }
        Self {
            name: name.unwrap_or_else(|| format_ident!("{}State", sm_name)),
            vis: vis.unwrap_or(sm_vis),
            attrs: attrs
                .into_iter()
                .map(|meta| Attribute {
                    pound_token: Default::default(),
                    style: syn::AttrStyle::Outer,
                    bracket_token: Default::default(),
                    meta,
                })
                .collect(),
        }
    }
}

struct State {
    /// Name of the variant
    name: Ident,
    /// Field of the variant
    content: Type,
    /// list of infallible transitions
    to: Vec<Dest>,
    /// list of fallible transitions
    try_to: Vec<Dest>,
    /// derive an `is_*` method
    is: bool,
    /// derive an `as_*` method
    r#as: bool,
    /// derive an `as_*_mut` method
    as_mut: bool,
    /// derive an `try_into_*` method
    try_into: bool,
}
impl State {
    fn from_def(
        StateDef {
            ident,
            fields,
            to,
            try_to,
            is,
            r#as,
            as_mut,
            try_into,
        }: StateDef,
        sm_is: bool,
        sm_as: bool,
        sm_as_mut: bool,
        sm_try_into: bool,
    ) -> Self {
        let content = fields.fields.into_iter().next().unwrap();
        Self {
            to: to
                .0
                .into_iter()
                .map(|d| Dest::from_def(d, &content, false))
                .collect(),
            try_to: try_to
                .0
                .into_iter()
                .map(|d| Dest::from_def(d, &content, true))
                .collect(),
            name: ident,
            content,
            is: is.unwrap_or(sm_is),
            r#as: r#as.unwrap_or(sm_as),
            as_mut: as_mut.unwrap_or(sm_as_mut),
            try_into: try_into.unwrap_or(sm_try_into),
        }
    }
}

struct Dest {
    to: Ident,
    fun: ExprPath,
}
impl Dest {
    fn from_def(DestDef { to, fun }: DestDef, from: &Type, fallible: bool) -> Dest {
        Self {
            fun: fun.unwrap_or_else(|| {
                let fn_name = if fallible {
                    format_ident!(
                        "try_into_{}",
                        to.to_string().from_case(Case::Pascal).to_case(Case::Snake)
                    )
                } else {
                    format_ident!(
                        "try_into_{}",
                        to.to_string().from_case(Case::Pascal).to_case(Case::Snake)
                    )
                };
                ExprPath {
                    attrs: vec![],
                    qself: Some(syn::QSelf {
                        lt_token: Default::default(),
                        ty: Box::new(from.clone()),
                        position: 0,
                        as_token: None,
                        gt_token: Default::default(),
                    }),
                    path: Path {
                        leading_colon: None,
                        segments: Punctuated::from_iter([PathSegment {
                            ident: fn_name,
                            arguments: syn::PathArguments::None,
                        }]),
                    },
                }
            }),
            to,
        }
    }
}

impl From<StateMachineDef> for StateMachine {
    fn from(
        StateMachineDef {
            ident,
            vis,
            data,
            states_enum,
            is,
            r#as,
            as_mut,
            try_into,
        }: StateMachineDef,
    ) -> Self {
        let is = is.unwrap_or(true);
        let r#as = r#as.unwrap_or(true);
        let as_mut = as_mut.unwrap_or(true);
        let try_into = try_into.unwrap_or(true);
        Self {
            states_enum: StatesEnum::from_def(states_enum, &ident, vis),
            ident,
            states: data
                .take_enum()
                .unwrap()
                .into_iter()
                .map(|s| State::from_def(s, is, r#as, as_mut, try_into))
                .collect(),
        }
    }
}

impl ToTokens for StateMachine {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let StateMachine {
            ident: sm_name,
            states,
            states_enum,
        } = self;
        // state enum
        {
            let StatesEnum { name, vis, attrs } = states_enum;
            // enum definition
            let state_names = states.iter().map(|State { name, .. }| name);
            quote!(
                #[automatically_derived]
                #(#attrs)*
                #vis enum #name {
                    #(#state_names),*
                }
            )
            .to_tokens(tokens);
            // getter
            let state_names = states.iter().map(|State { name, .. }| name);
            quote!(
                #[automatically_derived]
                impl #sm_name {
                    pub fn state(&self) -> #name {
                        match self {
                            #(Self::#state_names(_)=>#name::#state_names),*
                        }
                    }
                }
            )
            .to_tokens(tokens);
        }
    }
}

pub fn state_machine(input: &DeriveInput) -> TokenStream {
    match StateMachineDef::from_derive_input(input) {
        Ok(def) => StateMachine::from(def).into_token_stream(),
        Err(err) => err.write_errors(),
    }
}
