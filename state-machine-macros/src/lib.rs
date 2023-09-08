#![feature(iterator_try_collect)]

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod derive;

#[proc_macro_derive(StateMachine, attributes(state, state_machine))]
pub fn state_machine(item: TokenStream) -> TokenStream {
    derive::state_machine(&parse_macro_input!(item as DeriveInput)).into()
}
