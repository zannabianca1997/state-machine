use std::collections::{BTreeMap, BTreeSet};

use convert_case::{Case, Casing};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{Type, Visibility};

use self::parse::parse;

mod parse;

/// A definition of a state machine
#[derive(Debug, Clone)]
struct StateMachine {
    /// The name of the state machine enum
    name: Ident,
    /// The trait to generate
    ext_trait: ExtTrait,
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
}

pub(crate) fn state_machine(input: &syn::DeriveInput) -> TokenStream {
    let StateMachine {
        name,
        ext_trait: ExtTrait {
            name: tr_name,
            vis: tr_vis,
        },
        states,
    } = match parse(&input) {
        Ok(sm) => sm,
        Err(err) => return err,
    };

    // generating the functions and trait items
    let mut tr_items = TokenStream::new();
    let mut impl_items = TokenStream::new();

    for State {
        name: from_name,
        content: from_content,
        to,
        try_to,
    } in states.values()
    {
        let from_name_snake = from_name.to_string().to_case(Case::Snake);
        // is_*, as_*, try_into_*, new_* methods
        {
            let is_fn_name = format_ident!("is_{from_name_snake}");
            let as_fn_name = format_ident!("as_{from_name_snake}");
            let as_mut_fn_name = format_ident!("as_{from_name_snake}_mut");
            quote!(
                fn #is_fn_name(&self) -> bool;
                fn #as_fn_name(&self) -> ::std::option::Option<&#from_content>;
                fn #as_mut_fn_name(&mut self) -> ::std::option::Option<&mut #from_content>;
            )
            .to_tokens(&mut tr_items);
            quote!(
                fn #is_fn_name(&self) -> bool {
                    ::std::matches!(self, #name::#from_name(_))
                }
                fn #as_fn_name(&self) -> ::std::option::Option<&#from_content> {
                    if let #name::#from_name(content) = self {
                        ::std::option::Option::Some(content)
                    } else {
                        ::std::option::Option::None
                    }
                }
                fn #as_mut_fn_name(&mut self) -> ::std::option::Option<&mut #from_content> {
                    if let #name::#from_name(content) = self {
                        ::std::option::Option::Some(content)
                    } else {
                        ::std::option::Option::None
                    }
                }
            )
            .to_tokens(&mut impl_items)
        }
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
            let fn_return = quote!(::std::option::Option<()>);

            // fn declaration
            quote!(
                fn #fn_name(&mut self)->#fn_return;
            )
            .to_tokens(&mut tr_items);

            // fn impl
            quote!(
                fn #fn_name(&mut self)->#fn_return {
                    let mut res = ::std::option::Option::None;
                    ::state_machine::take_mut::take(self, |this| {
                        if let #name::#from_name(content) = this {
                            res = ::std::option::Option::Some(());
                            #name::#dest_name(::state_machine::TransitionTo::<#dest_content>::transition(content))
                        } else {
                            res = ::std::option::Option::None;
                            this
                        }
                    });
                    res
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
            let fn_return = quote!(
                ::std::option::Option<
                    ::std::result::Result<
                        (),
                        <#from_content as ::state_machine::TryTransitionTo<#dest_content>>::Error
                    >
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
                    let mut res = ::std::option::Option::None;
                    ::state_machine::take_mut::take(self, |this| {
                        if let #name::#from_name(content) = this {
                            match ::state_machine::TryTransitionTo::<#dest_content>::try_transition(&content) {
                                ::std::result::Result::Ok(f) => {
                                    res = ::std::option::Option::Some(::std::result::Result::Ok(()));
                                    #name::#dest_name(f(content))
                                }
                                ::std::result::Result::Err(err) => {
                                    res = ::std::option::Option::Some(::std::result::Result::Err(err));
                                    #name::#from_name(content)
                                }
                            }
                        } else {
                            res = ::std::option::Option::None;
                            this
                        }
                    });
                    res
                }
            )
            .to_tokens(&mut impl_items);
        }
    }

    quote!(
        #[automatically_derived]
        #tr_vis trait #tr_name {
            #tr_items
        }

        #[automatically_derived]
        impl #tr_name for #name {
            #impl_items
        }
    )
    .into_token_stream()
}
