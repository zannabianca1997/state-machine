use convert_case::{Case, Casing};
use darling::{
    ast::{Data, Fields, NestedMeta},
    FromDeriveInput, FromMeta, FromVariant,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_quote, punctuated::Punctuated, Attribute, DeriveInput, Expr, ExprPath, Ident, Meta,
    MetaList, MetaNameValue, Path, PathSegment, Type, Visibility,
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
    state_enum: StateEnumDef,
    /// Data for the state trait
    #[darling(default)]
    state_trait: StateTraitDef,
    /// Data for the wrong state error
    #[darling(default)]
    wrong_state_error: WrongStateErrorDef,
    /// derive `is_*` methods
    is: Option<bool>,
    /// derive `as_*` methods
    r#as: Option<bool>,
    /// derive `as_mut_*` methods
    as_mut: Option<bool>,
    /// derive `try_into_*` methods
    try_into: Option<bool>,
    /// default error type for fallible conversions
    error: Option<Type>,
    /// derive `state_to_*` metods
    state_to: Option<bool>,
}

#[derive(Debug, FromMeta, Default)]
struct StateEnumDef {
    /// Name of the state enum
    name: Option<Ident>,
    /// Visibility of the state enum
    vis: Option<Visibility>,
    /// Attributes of the state enum (if no derive than `derive(Debug, Clone, Copy, Eq, PartialEq, Hash)` is added)
    #[darling(multiple)]
    attrs: Vec<Meta>,
}
#[derive(Debug, FromMeta, Default)]
struct StateTraitDef {
    /// Name of the state trait
    name: Option<Ident>,
    /// Visibility of the state trait
    vis: Option<Visibility>,
}

#[derive(Debug, FromMeta, Default)]
struct WrongStateErrorDef {
    /// Name of the state enum
    name: Option<Ident>,
    /// Visibility of the state enum
    vis: Option<Visibility>,
    /// Attributes of the state enum (if no derive than `derive(Debug, Clone, Copy, Eq, PartialEq, Hash)` is added)
    #[darling(multiple)]
    attrs: Vec<Meta>,
    /// If ::std::Error is derived
    std_error: Option<bool>,
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
    /// derive `is_*` methods
    is: Option<bool>,
    /// derive `as_*` methods
    r#as: Option<bool>,
    /// derive `as_*_mut` methods
    as_mut: Option<bool>,
    /// derive `try_into_*` methods
    try_into: Option<bool>,
    /// derive `state_to_*` metod
    state_to: Option<bool>,
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
    fun: Option<Expr>,
    err: Option<Type>,
}
impl FromMeta for DestDef {
    fn from_nested_meta(item: &NestedMeta) -> darling::Result<Self> {
        (match item {
            NestedMeta::Meta(Meta::Path(path)) if let Some(ident) = path.get_ident() => Ok(DestDef{to:ident.clone(), fun:None, err:None}),
            NestedMeta::Meta(Meta::NameValue(MetaNameValue{ path, value:fun,.. })) if let Some(name) = path.get_ident() => Ok(DestDef{to:name.clone(), fun:Some(fun.clone()),err:None}),
            NestedMeta::Meta(meta @ Meta::List(MetaList { path, .. }))if let Some(name) = path.get_ident()=> {
                #[derive(FromMeta)]
                struct DestData {
                    fun: Option<Expr>,
                    err: Option<Type>,
                }

                 DestData::from_meta(meta).map(|DestData { fun, err }| DestDef { to: name.clone(), fun, err })
            }
        _ => Err(darling::Error::custom("Expected `State`, `State = path::to::fun` or `State(fun=.., err=..)`")),
    })
        .map_err(|e| e.with_span(item))
    }
}
#[derive(Debug)]
struct FallibleDestDef {
    to: Ident,
    fun: Option<Expr>,
    err: Option<Type>,
}
impl FromMeta for FallibleDestDef {
    fn from_nested_meta(item: &NestedMeta) -> darling::Result<Self> {
        (match item {
            NestedMeta::Meta(Meta::Path(path)) if let Some(ident) = path.get_ident() => Ok(FallibleDestDef{to:ident.clone(), fun:None, err:None}),
            NestedMeta::Meta(Meta::NameValue(MetaNameValue{ path, value:fun,.. })) if let Some(name) = path.get_ident() => Ok(FallibleDestDef{to:name.clone(), fun:Some(fun.clone()),err:None}),
            NestedMeta::Meta(meta @ Meta::List(MetaList { path, .. }))if let Some(name) = path.get_ident()=> {
                #[derive(FromMeta)]
                struct DestData {
                    fun: Option<Expr>,
                    err: Option<Type>,
                }

                 DestData::from_meta(meta).map(|DestData { fun, err }| FallibleDestDef { to: name.clone(), fun, err })
            }
        _ => Err(darling::Error::custom("Expected `State`, `State = path::to::fun` or `State(fun=.., err=..)`")),
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
    state_enum: StatesEnum,
    /// Data for the state content trait
    state_trait: StateTrait,
    /// Data for the error in case of wrong state
    wrong_state_error: WrongStateError,
}

struct WrongStateError {
    /// Name of the wrong state error
    name: Ident,
    /// Visibility of wrong state error
    vis: Visibility,
    /// Attributes of the state enum (if no derive than `derive(Debug, Clone, Copy, Eq, PartialEq, Hash)` is added)
    attrs: Vec<Attribute>,
    /// If `::std::Error` is to implement
    std_error: bool,
}
impl WrongStateError {
    fn from_def(
        WrongStateErrorDef {
            name,
            vis,
            mut attrs,
            std_error,
        }: WrongStateErrorDef,
        sm_name: &Ident,
        sm_vis: Visibility,
    ) -> WrongStateError {
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
            name: name.unwrap_or_else(|| format_ident!("{sm_name}WrongState")),
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
            std_error: std_error.unwrap_or(true),
        }
    }
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
        StateEnumDef {
            name,
            vis,
            mut attrs,
        }: StateEnumDef,
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
            name: name.unwrap_or_else(|| format_ident!("{sm_name}State")),
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

struct StateTrait {
    /// Name of the state trait
    name: Ident,
    /// Visibility of the state trait
    vis: Visibility,
}
impl StateTrait {
    fn from_def(
        StateTraitDef { name, vis }: StateTraitDef,
        sm_name: &Ident,
        sm_vis: Visibility,
    ) -> Self {
        Self {
            name: name.unwrap_or_else(|| format_ident!("{sm_name}StateContent")),
            vis: vis.unwrap_or(sm_vis),
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
    try_to: Vec<FallibleDest>,
    /// derive `is_*` method
    is: bool,
    /// derive `as_*` method
    r#as: bool,
    /// derive `as_*_mut` method
    as_mut: bool,
    /// derive `try_into_*` method
    try_into: bool,
    /// derive `state_to_*` method
    state_to: bool,
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
            state_to,
        }: StateDef,
        sm_is: bool,
        sm_as: bool,
        sm_as_mut: bool,
        sm_try_into: bool,
        sm_err: Option<&Type>,
        sm_state_to: bool,
    ) -> syn::Result<Self> {
        let content = fields.fields.into_iter().next().unwrap();
        Ok(Self {
            to: to
                .0
                .into_iter()
                .map(|d| Dest::from_def(d, &content))
                .collect(),
            try_to: try_to
                .0
                .into_iter()
                .map(|d| FallibleDest::from_def(d, &content, sm_err))
                .collect_syn_errors()?,
            name: ident,
            content,
            is: is.unwrap_or(sm_is),
            r#as: r#as.unwrap_or(sm_as),
            as_mut: as_mut.unwrap_or(sm_as_mut),
            try_into: try_into.unwrap_or(sm_try_into),
            state_to: state_to.unwrap_or(sm_state_to),
        })
    }
}

struct Dest {
    to: Ident,
    fun: Expr,
}
impl Dest {
    fn from_def(DestDef { to, fun, err }: DestDef, from: &Type) -> Self {
        Self {
            fun: fun.unwrap_or_else(|| {
                let fn_name = format_ident!(
                    "to_{}",
                    to.to_string().from_case(Case::Pascal).to_case(Case::Snake)
                );
                Expr::Path(ExprPath {
                    attrs: vec![],
                    qself: Some(syn::QSelf {
                        lt_token: Default::default(),
                        ty: Box::new(from.clone()),
                        position: 0,
                        as_token: None,
                        gt_token: Default::default(),
                    }),
                    path: Path {
                        leading_colon: Some(Default::default()),
                        segments: Punctuated::from_iter([PathSegment {
                            ident: fn_name,
                            arguments: syn::PathArguments::None,
                        }]),
                    },
                })
            }),
            to,
        }
    }
}
struct FallibleDest {
    to: Ident,
    fun: Expr,
    err: Type,
}
impl FallibleDest {
    fn from_def(
        DestDef { to, fun, err }: DestDef,
        from: &Type,
        sm_err: Option<&Type>,
    ) -> syn::Result<Self> {
        Ok(Self {
            fun: fun.unwrap_or_else(|| {
                let fn_name = format_ident!(
                    "try_to_{}",
                    to.to_string().from_case(Case::Pascal).to_case(Case::Snake)
                );
                Expr::Path(ExprPath {
                    attrs: vec![],
                    qself: Some(syn::QSelf {
                        lt_token: Default::default(),
                        ty: Box::new(from.clone()),
                        position: 0,
                        as_token: None,
                        gt_token: Default::default(),
                    }),
                    path: Path {
                        leading_colon: Some(Default::default()),
                        segments: Punctuated::from_iter([PathSegment {
                            ident: fn_name,
                            arguments: syn::PathArguments::None,
                        }]),
                    },
                })
            }),
            err: match err.or_else(|| sm_err.cloned()) {
                Some(err) => err,
                None => {
                    return Err(syn::Error::new_spanned(
                        to,
                        "Error type is unspecified, and state machine global error is too",
                    ));
                }
            },
            to,
        })
    }
}

impl TryFrom<StateMachineDef> for StateMachine {
    type Error = syn::Error;
    fn try_from(
        StateMachineDef {
            ident,
            vis,
            data,
            state_enum,
            state_trait,
            wrong_state_error,
            is,
            r#as,
            as_mut,
            try_into,
            error,
            state_to,
        }: StateMachineDef,
    ) -> Result<Self, Self::Error> {
        let is = is.unwrap_or(true);
        let r#as = r#as.unwrap_or(true);
        let as_mut = as_mut.unwrap_or(true);
        let try_into = try_into.unwrap_or(true);
        let state_to = state_to.unwrap_or(true);
        Ok(Self {
            state_enum: StatesEnum::from_def(state_enum, &ident, vis.clone()),
            state_trait: StateTrait::from_def(state_trait, &ident, vis.clone()),
            wrong_state_error: WrongStateError::from_def(wrong_state_error, &ident, vis),
            ident,
            states: data
                .take_enum()
                .unwrap()
                .into_iter()
                .map(|s| State::from_def(s, is, r#as, as_mut, try_into, error.as_ref(), state_to))
                .collect_syn_errors()?,
        })
    }
}

impl ToTokens for StateMachine {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let StateMachine {
            ident: sm_name,
            states,
            state_enum,
            state_trait,
            wrong_state_error,
        } = self;
        // collector for the function to create in the machine
        let mut sm_impl = TokenStream::new();
        // state enum
        {
            let StatesEnum { name, vis, attrs } = state_enum;
            // enum definition
            let state_names = states.iter().map(|State { name, .. }| name);
            quote!(
                #(#attrs)*
                #vis enum #name {
                    #(#state_names),*
                }
            )
            .to_tokens(tokens);
        }
        // state trait
        {
            let StateTrait { name, vis } = state_trait;
            let StatesEnum {
                name: state_enum, ..
            } = state_enum;
            // trait definition
            quote!(
                #[automatically_derived]
                #vis trait #name {
                    const STATE: #state_enum;
                }
            )
            .to_tokens(tokens);
        }
        // wrong state error
        {
            let WrongStateError {
                name,
                vis,
                attrs,
                std_error,
            } = wrong_state_error;
            let StatesEnum {
                name: state_enum, ..
            } = state_enum;
            // trait definition
            quote!(
                #(#attrs)*
                #vis struct #name {
                    method: &'static str,
                    valid: &'static [#state_enum],
                    found: #state_enum
                }
            )
            .to_tokens(tokens);
            if *std_error {
                quote!(
                    #[automatically_derived]
                    impl ::std::error::Error for #name { }
                    #[automatically_derived]
                    impl ::std::fmt::Display for #name {
                        fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                            // assume there is always at least a valid state
                            write!(f, "Tried to access method {} during the state {:?} (valid states are [{:?}", self.method, self.found, self.valid[0])?;
                            for state in &self.valid[1..] {
                                write!(f, ", {:?}", state)?;
                            }
                            write!(f, "])")
                        }
                    }
                )
                .to_tokens(tokens);
            }
        }
        // per state functions and implementations
        for State {
            name,
            content,
            to,
            try_to,
            is,
            r#as,
            as_mut,
            try_into,
            state_to,
        } in states
        {
            // Trait impl
            let StateTrait {
                name: state_trait, ..
            } = state_trait;
            let StatesEnum {
                name: state_enum, ..
            } = state_enum;
            let WrongStateError {
                name: wrong_state_error,
                ..
            } = wrong_state_error;

            quote!(
                #[automatically_derived]
                impl #state_trait for #content {
                    const STATE: #state_enum = #state_enum::#name;
                }
            )
            .to_tokens(tokens);

            let snake_case_name = name
                .to_string()
                .from_case(Case::Pascal)
                .to_case(Case::Snake);
            if *is {
                let fn_name = format_ident!("is_{snake_case_name}");
                quote!(
                    pub fn #fn_name(&self)->bool {
                        self.state() == #state_enum::#name
                    }
                )
                .to_tokens(&mut sm_impl);
            }
            if *r#as {
                let fn_name = format_ident!("as_{snake_case_name}");
                let fn_string_name = fn_name.to_string();
                quote!(
                    pub fn #fn_name(&self)->::std::result::Result<&#content, #wrong_state_error> {
                        if let Self::#name(content) = self {
                            ::std::result::Result::Ok(content)
                        } else {
                            ::std::result::Result::Err(#wrong_state_error {
                                method: #fn_string_name,
                                valid: &[#state_enum::#name],
                                found: self.state()
                            })
                        }
                    }
                )
                .to_tokens(&mut sm_impl);
            }
            if *as_mut {
                let fn_name = format_ident!("as_{snake_case_name}_mut");
                let fn_string_name = fn_name.to_string();
                quote!(
                    pub fn #fn_name(&mut self)->::std::result::Result<&mut #content, #wrong_state_error> {
                        if let Self::#name(content) = self {
                            ::std::result::Result::Ok(content)
                        } else {
                            ::std::result::Result::Err(#wrong_state_error {
                                method: #fn_string_name,
                                valid: &[#state_enum::#name],
                                found: self.state()
                            })
                        }
                    }
                )
                .to_tokens(&mut sm_impl);
            }
            if *try_into {
                let fn_name = format_ident!("try_into_{snake_case_name}");
                let fn_string_name = fn_name.to_string();
                quote!(
                    pub fn #fn_name(self) -> ::std::result::Result<#content, (Self, #wrong_state_error)> {
                        if let Self::#name(content) = self {
                            ::std::result::Result::Ok(content)
                        } else {
                            let found = self.state();
                            ::std::result::Result::Err((self, #wrong_state_error {
                                method: #fn_string_name,
                                valid: &[#state_enum::#name],
                                found
                            }))
                        }
                    }
                )
                .to_tokens(&mut sm_impl);
            }

            // infallible transitions
            for Dest { to, fun } in to {
                let fn_name = format_ident!(
                    "from_{snake_case_name}_to_{}",
                    to.to_string().from_case(Case::Pascal).to_case(Case::Snake)
                );
                let fn_string_name = fn_name.to_string();
                let fallible_fn_name = format_ident!(
                    "try_from_{snake_case_name}_to_{}",
                    to.to_string().from_case(Case::Pascal).to_case(Case::Snake)
                );
                let fallible_fn_string_name = fallible_fn_name.to_string();
                let as_mut = format_ident!("as_{snake_case_name}_mut");
                quote!(
                    pub fn #fn_name(&mut self) ->::std::result::Result<(),  #wrong_state_error> {
                        let content = ::std::mem::take(self.#as_mut().map_err(|mut err| {err.method = #fn_string_name; err})? );
                        *self = Self::#to((#fun)(content));
                        ::std::result::Result::Ok(())
                    }
                    pub fn #fallible_fn_name(&mut self) ->::std::result::Result<Result<(), ::std::convert::Infallible>,  #wrong_state_error> {
                        let content = ::std::mem::take(self.#as_mut().map_err(|mut err| {err.method = #fallible_fn_string_name; err})? );
                        *self = Self::#to((#fun)(content));
                        ::std::result::Result::Ok(::std::result::Result::Ok(()))
                    }
                )
                .to_tokens(&mut sm_impl);
            }

            // fallible transitions
            for FallibleDest { to, fun, err } in try_to {
                let fn_name = format_ident!(
                    "try_from_{snake_case_name}_to_{}",
                    to.to_string().from_case(Case::Pascal).to_case(Case::Snake)
                );
                let fn_string_name = fn_name.to_string();
                let as_mut = if *as_mut {
                    // using the as_*_mut impl
                    let fn_name = format_ident!("as_{snake_case_name}_mut");
                    quote!(self.#fn_name()?)
                } else {
                    // building a local as_*_mut
                    quote!(if let Self::#name(content) = self {
                        content
                    } else {
                        return ::std::result::Result::Err(#wrong_state_error {
                            method: #fn_string_name,
                            valid: &[#state_enum::#name],
                            found: self.state()
                        });
                    })
                };
                quote!(
                    pub fn #fn_name(&mut self) ->::std::result::Result<Result<(), #err>,  #wrong_state_error> {
                        let content = ::std::mem::take(#as_mut);
                        match (#fun)(content) {
                            ::std::result::Result::Ok(new_content) => {
                                *self = Self::#to(new_content);
                                ::std::result::Result::Ok(
                                    ::std::result::Result::Ok(())
                                )
                            },
                            ::std::result::Result::Err((content, err)) => {
                                *self = Self::#name(content);
                                ::std::result::Result::Ok(
                                    ::std::result::Result::Err(err)
                                )
                            }
                        }
                    }
                )
                .to_tokens(&mut sm_impl);
            }

            // global infallible state transition
            if *state_to {
                // All the states that can transition to this
                let sources: Vec<_> = states
                    .iter()
                    .filter_map(|State { name: from, to, .. }| {
                        if to.iter().any(|Dest { to, .. }| to == name) {
                            Some(from)
                        } else {
                            None
                        }
                    })
                    .collect();
                let transitions = sources.iter().map(|from| {
                    format_ident!(
                        "from_{}_to_{snake_case_name}",
                        from.to_string()
                            .from_case(Case::Pascal)
                            .to_case(Case::Snake)
                    )
                });
                let fn_name = format_ident!("state_to_{snake_case_name}");
                let fn_string_name = fn_name.to_string();
                quote!(
                    pub fn #fn_name(&mut self) ->::std::result::Result<(),  #wrong_state_error> {
                        match self {
                            #(Self::#sources(_)=>::std::result::Result::Ok(self.#transitions().unwrap()),)*
                            _=>::std::result::Result::Err(#wrong_state_error {
                                method: #fn_string_name,
                                valid: &[#(#state_enum::#sources),*],
                                found: self.state()
                            })
                        }
                    }
                )
                .to_tokens(&mut sm_impl);
            }
        }
        // Various impl functions
        {
            let StateTrait {
                name: state_trait, ..
            } = state_trait;
            let StatesEnum {
                name: state_enum, ..
            } = state_enum;
            let WrongStateError {
                name: wrong_state_error,
                ..
            } = wrong_state_error;
            // state getter
            let state_names = states.iter().map(|State { name, .. }| name);
            quote!(
                pub const fn state(&self) -> #state_enum {
                    match self {
                        #(Self::#state_names(_)=>#state_enum::#state_names),*
                    }
                }
            )
            .to_tokens(&mut sm_impl);
        }

        quote!(
            #[automatically_derived]
            impl #sm_name {
                #sm_impl
            }
        )
        .to_tokens(tokens)
    }
}

pub fn state_machine(input: &DeriveInput) -> TokenStream {
    match StateMachineDef::from_derive_input(input).map(StateMachine::try_from) {
        Ok(Ok(sm)) => sm.into_token_stream(),
        Ok(Err(err)) => err.into_compile_error(),
        Err(err) => err.write_errors(),
    }
}

trait CollectSynErrors<I> {
    fn collect_syn_errors<C>(self) -> syn::Result<C>
    where
        C: FromIterator<I>;
}

impl<T, I> CollectSynErrors<I> for T
where
    T: Iterator<Item = syn::Result<I>>,
{
    fn collect_syn_errors<C>(self) -> syn::Result<C>
    where
        C: FromIterator<I>,
    {
        let mut errors: Option<syn::Error> = None;
        let collected = self
            .filter_map(|r| match (r, &mut errors) {
                (Ok(_), Some(_)) => None,
                (Ok(item), None) => Some(item),
                (Err(err), Some(errs)) => {
                    errs.combine(err);
                    None
                }
                (Err(err), errs_none) => {
                    *errs_none = Some(err);
                    None
                }
            })
            .collect();
        if let Some(errors) = errors {
            Err(errors)
        } else {
            Ok(collected)
        }
    }
}
