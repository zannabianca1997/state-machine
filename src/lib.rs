//! # A Rust state machine
//!
//! ```
//! use state_machine::StateMachine;
//!
//! /// My state machine
//! #[derive(StateMachine)]
//! enum SM {
//!   #[state(to(B,C))]
//!   A(A),
//!   #[state(try_to(A = A::from_B))]
//!   B(B),
//!   #[state(to(A = C::into_A), try_into)]
//!   C(C)
//! }
//!
//! /// Data of the state A
//! struct A;
//!
//! /// Data of the state B
//! struct B;
//!
//! /// Data of the state C
//! struct C;
//!
//! assert_eq!(SM::A(A).state(), SMState::A)
//!
//! ```
#![feature(if_let_guard)]
#![feature(iterator_try_collect)]

use syn::parse_macro_input;
mod derive;

#[proc_macro_derive(StateMachine, attributes(state_machine, state))]
pub fn state_machine(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive::state_machine(&parse_macro_input!(item)).into()
}
