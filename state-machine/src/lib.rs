#![feature(return_position_impl_trait_in_trait)]
#![feature(never_type)]

use std::{
    error::Error,
    fmt::{Debug, Display},
};

use itertools::Itertools;
// reexport macros
pub use state_machine_macros::*;

// reexport dependencies
pub use take_mut;

/// Infallible transition to another state content
pub trait TransitionTo<Dest> {
    fn transition(self) -> Dest;
}

/// Blanket implementation for identity transform
impl<T> TransitionTo<T> for T {
    fn transition(self) -> T {
        self
    }
}

/// Fallible transition to another state content
pub trait TryTransitionTo<Dest> {
    type Error;
    fn try_transition(&self) -> Result<impl FnOnce(Self) -> Dest, Self::Error>
    where
        Self: Sized;
}

/// Blanket implementations for infallible as fallible transforms
impl<T, D> TryTransitionTo<D> for T
where
    T: TransitionTo<D>,
{
    type Error = !;

    fn try_transition(&self) -> Result<impl FnOnce(Self) -> D, Self::Error>
    where
        Self: Sized,
    {
        Ok(T::transition)
    }
}

/// Error for interacting with a state machine in the wrong state
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WrongStateError<StateEnum>
where
    StateEnum: 'static,
{
    pub method: &'static str,
    pub found: StateEnum,
    pub allowed: &'static [StateEnum],
}
impl<S> Display for WrongStateError<S>
where
    S: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Cannot call {} in state {} [allowed: {}]",
            self.method,
            self.found,
            self.allowed.iter().format(", ")
        )
    }
}
impl<S> Error for WrongStateError<S> where S: Debug + Display {}

/// Error for interacting with a state machine in the wrong state, using generic parameters
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GenericFromToError<StateEnum>
where
    StateEnum: 'static,
{
    WrongState {
        from: StateEnum,
        found: StateEnum,
    },
    NoTransition {
        from: StateEnum,
        to: StateEnum,
        fallible: bool,
    },
}
impl<S> Display for GenericFromToError<S>
where
    S: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericFromToError::WrongState { from, found } => write!(
                f,
                "Machine in the wrong state: requested {from}, found {found}"
            ),
            GenericFromToError::NoTransition { from, to, fallible } => {
                write!(f, "Impossible transition from {from} to {to} requested")?;
                if *fallible {
                    write!(f, "(a fallible transition exist)")?;
                }
                Ok(())
            }
        }
    }
}
impl<S> Error for GenericFromToError<S> where S: Debug + Display {}

/// Error for interacting with a state machine in the wrong state, using generic parameters
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GenericToError<StateEnum>
where
    StateEnum: 'static,
{
    NoTransition {
        found: StateEnum,
        to: StateEnum,
        fallible: bool,
    },
}
impl<S> Display for GenericToError<S>
where
    S: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericToError::NoTransition {
                found,
                to,
                fallible,
            } => {
                write!(f, "Impossible transition from {found} to {to}")?;
                if *fallible {
                    write!(f, "(a fallible transition exist)")?;
                }
                Ok(())
            }
        }
    }
}
impl<S> Error for GenericToError<S> where S: Debug + Display {}
