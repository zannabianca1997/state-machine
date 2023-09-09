use std::collections::{BTreeMap, BTreeSet};

use convert_case::{Case, Casing};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{Meta, Type, Visibility};

use self::parse::parse;

mod parse;

/// A definition of a state machine
#[derive(Debug, Clone)]
struct StateMachine {
    /// The name of the state machine enum
    name: Ident,
    /// The trait to generate
    ext_trait: ExtTrait,
    /// The enum of states to generate
    state_enum: StateEnum,
    /// The states of the machine
    states: BTreeMap<Ident, State>,
}

/// The trait containing all the state machine functions
#[derive(Debug, Clone)]
struct ExtTrait {
    /// The name of the trait
    name: Ident,
    /// Visibility of the trait
    vis: Visibility,
}
/// The enumeration of the state machine states
#[derive(Debug, Clone)]
struct StateEnum {
    /// The name of the enum
    name: Ident,
    /// Visibility of the enum
    vis: Visibility,
    /// Attributes passed on the enum
    attrs: Vec<Meta>,
}

impl StateEnum {
    fn to_tokens<'v>(
        &self,
        mut tokens: &mut TokenStream,
        variants: impl Iterator<Item = &'v Ident>,
    ) {
        let StateEnum { name, vis, attrs } = self;
        let variants: Vec<_> = variants.collect();
        let variants_names = variants.iter().map(|v| v.to_string());

        quote!(
            #(#attrs)*
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            #vis enum #name {
                #(#variants),*
            }

            #[automatically_derived]
            impl ::std::fmt::Display for #name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    ::std::write!(
                        f,
                        "{}",
                        match self {
                            #( #name::#variants => #variants_names, )*
                        }
                    )
                }
            }
        )
        .to_tokens(&mut tokens)
    }
}

/// A state of the machine
#[derive(Debug, Clone)]
struct State {
    /// The name of the variant
    name: Ident,
    /// The content of the state, if any
    content: Type,
    /// The infallible transitions this state can undergo
    to: BTreeSet<Ident>,
    /// The fallible transitions this state can undergo
    try_to: BTreeSet<Ident>,
    /// The error for the fallible transition from any state to this one
    error: Option<Type>,
}

pub(crate) fn state_machine(input: &syn::DeriveInput) -> TokenStream {
    let StateMachine {
        name,
        ext_trait: ExtTrait {
            name: tr_name,
            vis: tr_vis,
        },
        states,
        state_enum,
    } = match parse(&input) {
        Ok(sm) => sm,
        Err(err) => return err,
    };
    let se_name = &state_enum.name;

    // generating the functions and trait items
    let mut tr_items = TokenStream::new();
    let mut impl_items = TokenStream::new();

    {
        let variants_1 = states.keys();
        let variants_2 = states.keys();
        quote!(
            type State;
            fn state(&self) -> Self::State;
        )
        .to_tokens(&mut tr_items);
        quote!(
            type State = #se_name;
            fn state(&self) -> Self::State {
                match self {
                    #(Self::#variants_1(_)=>#se_name::#variants_2,)*
                }

            }
        )
        .to_tokens(&mut impl_items);
    }

    // state-specific methods
    for State {
        name: from_name,
        content: from_content,
        to,
        try_to,
        error,
    } in states.values()
    {
        let from_name_snake = from_name.to_string().to_case(Case::Snake);
        // is_*, as_*, try_into_*, new_* methods

        let is_fn_name = format_ident!("is_{from_name_snake}");
        let as_fn_name = format_ident!("as_{from_name_snake}");
        let as_mut_fn_name = format_ident!("as_{from_name_snake}_mut");
        let as_fn_name_str = as_fn_name.to_string();
        let as_mut_fn_name_str = as_mut_fn_name.to_string();
        quote!(
            fn #is_fn_name(&self) -> bool;
            fn #as_fn_name(&self) -> ::std::result::Result<&#from_content, ::state_machine::WrongStateError<Self::State>>;
            fn #as_mut_fn_name(&mut self) -> ::std::result::Result<&mut #from_content, ::state_machine::WrongStateError<Self::State>>;
        )
        .to_tokens(&mut tr_items);
        quote!(
            fn #is_fn_name(&self) -> bool {
                ::std::matches!(self, #name::#from_name(_))
            }
            fn #as_fn_name(&self) -> ::std::result::Result<&#from_content, ::state_machine::WrongStateError<Self::State>> {
                if let #name::#from_name(content) = self {
                    ::std::result::Result::Ok(content)
                } else {
                    ::std::result::Result::Err(::state_machine::WrongStateError {
                        method: #as_fn_name_str,
                        found: self.state(),
                        allowed: &[#se_name::#from_name],
                    })
                }
            }
            fn #as_mut_fn_name(&mut self) -> ::std::result::Result<&mut #from_content, ::state_machine::WrongStateError<Self::State>> {
                if let #name::#from_name(content) = self {
                    ::std::result::Result::Ok(content)
                } else {
                    ::std::result::Result::Err(::state_machine::WrongStateError {
                        method: #as_mut_fn_name_str,
                        found: self.state(),
                        allowed: &[#se_name::#from_name],
                    })
                }
            }
        )
        .to_tokens(&mut impl_items);

        // infallible transformations
        for dest in to {
            let State {
                name: dest_name,
                content: dest_content,
                ..
            } = states
                .get(dest)
                .expect("The parsing should refuse unknown variants");
            let fn_name = format_ident!(
                "from_{from_name_snake}_to_{}",
                dest_name.to_string().to_case(Case::Snake)
            );
            let fn_name_str = fn_name.to_string();
            let fn_return = quote!(::std::result::Result<&mut Self, ::state_machine::WrongStateError<Self::State>>);

            // fn declaration
            quote!(
                fn #fn_name(&mut self)->#fn_return;
            )
            .to_tokens(&mut tr_items);

            // fn impl
            quote!(
                fn #fn_name(&mut self)->#fn_return {
                    if self.state() == #se_name::#from_name {
                        ::state_machine::take_mut::take(self, |this| {
                            let #name::#from_name(content) = this else {unreachable!()};
                            #name::#dest_name(::state_machine::TransitionTo::<#dest_content>::transition(content))
                        });
                        ::std::result::Result::Ok(self)
                    } else {
                        ::std::result::Result::Err(::state_machine::WrongStateError {
                            method: #fn_name_str,
                            found: self.state(),
                            allowed: &[#se_name::#from_name],
                        })
                    }
                }
            )
            .to_tokens(&mut impl_items);
        }
        // fallible transformations
        for dest in try_to {
            let State {
                name: dest_name,
                content: dest_content,
                ..
            } = states
                .get(dest)
                .expect("The parsing should refuse unknown variants");
            let fn_name = format_ident!(
                "try_from_{from_name_snake}_to_{}",
                dest_name.to_string().to_case(Case::Snake)
            );
            let fn_name_str = fn_name.to_string();
            let fn_return = quote!(
                ::std::result::Result<
                    ::std::result::Result<
                        &mut Self,
                        <#from_content as ::state_machine::TryTransitionTo<#dest_content>>::Error
                    >,
                    ::state_machine::WrongStateError<Self::State>
                >
            );

            // fn declaration
            quote!(
                fn #fn_name(&mut self) -> #fn_return;
            )
            .to_tokens(&mut tr_items);

            // fn impl
            quote!(
                fn #fn_name(&mut self) -> #fn_return{
                    let content = self.#as_fn_name().map_err(|mut err| {err.method = #fn_name_str; err} )?;
                    let fun = match ::state_machine::TryTransitionTo::<#dest_content>::try_transition(content) {
                        ::std::result::Result::Ok(fun) => fun,
                        ::std::result::Result::Err(err) => return ::std::result::Result::Ok(::std::result::Result::Err(err)),
                    };
                    ::state_machine::take_mut::take(self, |this| {
                        let #name::#from_name(content) = this else {unreachable!()};
                        #name::#dest_name((fun)(content))
                    });
                    ::std::result::Result::Ok(::std::result::Result::Ok(self))
                }
            )
            .to_tokens(&mut impl_items);
        }

        // global infallible transform to this state
        {
            // some renaming to make code cleaner
            let to_name = from_name;
            let to_name_snake = &from_name_snake;

            let fn_name = format_ident!("to_{to_name_snake}");
            let fn_name_str = fn_name.to_string();
            let fn_return = quote!(::std::result::Result<&mut Self, ::state_machine::WrongStateError<Self::State>>);

            let from_names: Vec<_> = states
                .values()
                .filter_map(|State { name, to, .. }| to.contains(to_name).then_some(name))
                .collect();
            let from_funs = from_names.iter().map(|n| {
                format_ident!(
                    "from_{}_to_{to_name_snake}",
                    n.to_string().to_case(Case::Snake)
                )
            });

            // fn declaration
            quote!(
                fn #fn_name(&mut self)->#fn_return;
            )
            .to_tokens(&mut tr_items);

            // fn impl
            quote!(
                fn #fn_name(&mut self)->#fn_return {
                    match self {
                        #(Self::#from_names(_) => ::std::result::Result::Ok(self.#from_funs().unwrap()),)*
                        _ => ::std::result::Result::Err(::state_machine::WrongStateError {
                            method: #fn_name_str,
                            found: self.state(),
                            allowed: &[#(#se_name::#from_names),*],
                        }),
                    }
                }
            )
            .to_tokens(&mut impl_items);
        }

        // global fallible transform to this state
        if let Some(error) = error {
            // some renaming to make code cleaner
            let to_name = from_name;
            let to_name_snake = &from_name_snake;

            let fn_name = format_ident!("try_to_{to_name_snake}");
            let fn_name_str = fn_name.to_string();
            let fn_return = quote!(::std::result::Result<::std::result::Result<&mut Self, #error>, ::state_machine::WrongStateError<Self::State>>);

            let from_names: Vec<_> = states
                .values()
                .filter_map(|State { name, try_to, .. }| try_to.contains(to_name).then_some(name))
                .collect();
            let from_funs = from_names.iter().map(|n| {
                format_ident!(
                    "try_from_{}_to_{to_name_snake}",
                    n.to_string().to_case(Case::Snake)
                )
            });

            // fn declaration
            quote!(
                fn #fn_name(&mut self)->#fn_return;
            )
            .to_tokens(&mut tr_items);

            // fn impl
            quote!(
                fn #fn_name(&mut self)->#fn_return {
                    match self {
                        #(Self::#from_names(_) => ::std::result::Result::Ok(self.#from_funs().unwrap().map_err(::std::convert::Into::into)),)*
                        _ => ::std::result::Result::Err(::state_machine::WrongStateError {
                            method: #fn_name_str,
                            found: self.state(),
                            allowed: &[#(#se_name::#from_names),*],
                        }),
                    }
                }
            )
            .to_tokens(&mut impl_items);
        }
    }

    let mut tokens = quote!(
        #tr_vis trait #tr_name {
            #tr_items
        }

        #[automatically_derived]
        #[allow(unreachable_code)]
        impl #tr_name for #name {
            #impl_items
        }
    )
    .into_token_stream();
    state_enum.to_tokens(&mut tokens, states.keys());
    tokens
}
