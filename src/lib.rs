//! # A Rust state machine
//!
//! ```
//! use state_machine::StateMachine;
//!
//! /// My state machine
//! #[derive(Debug, PartialEq, Eq, StateMachine)]
//! #[state_machine(error="()")]
//! enum SM {
//!   #[state(to(B,C))]
//!   A(A),
//!   #[state(try_to(A = A::try_from_beta))]
//!   B(B),
//!   #[state(to(A = C::into_A))]
//!   C(C)
//! }
//!
//! /// Data of the state A
//! #[derive(Debug,PartialEq, Eq,Default)]
//! struct A;
//! impl A {
//!   fn to_b(&mut self) -> B {
//!     B
//!   }
//!   fn to_c(&mut self) -> C {
//!     C
//!   }
//!   fn try_from_beta(value: &mut B)-> Result<Self, ()> {
//!     Err(())
//!   }
//! }
//!
//! /// Data of the state B
//! #[derive(Debug,PartialEq, Eq,Default)]
//! struct B;
//!
//! /// Data of the state C
//! #[derive(Debug,PartialEq, Eq,Default)]
//! struct C;
//! impl C {
//!   fn into_A(&mut self)->A {
//!     A
//!   }
//! }
//!
//! assert_eq!(SM::A(A).state(), SMState::A);
//! assert!(SM::A(A).is_a());
//! assert!(!SM::A(A).is_b());
//! assert_eq!(SM::A(A).as_a(), Ok(&A));
//! assert_eq!(
//!     SM::A(A).as_b(),
//!     Err(SMWrongState {
//!         method: "as_b",
//!         valid: &[SMState::B],
//!         found: SMState::A
//!     })
//! );
//! assert_eq!(SM::A(A).try_into_a(), Ok(A));
//! assert_eq!(
//!     SM::A(A).try_into_b(),
//!     Err(SM::A(A))
//! );
//!
//! let mut sm = SM::A(A);
//! sm.state_to_c().unwrap();
//! assert!(sm.is_c());
//! sm.from_c_to_a().unwrap();
//! assert!(sm.is_a())
//! ```
#![feature(if_let_guard)]
#![feature(iterator_try_collect)]

use syn::parse_macro_input;
mod derive;

#[proc_macro_derive(StateMachine, attributes(state_machine, state))]
pub fn state_machine(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive::state_machine(&parse_macro_input!(item)).into()
}
