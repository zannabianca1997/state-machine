#![feature(return_position_impl_trait_in_trait)]

use std::{
    convert::Infallible,
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

impl<T, D> TryTransitionTo<D> for T
where
    T: TransitionTo<D>,
{
    type Error = Infallible;

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
    method: &'static str,
    found: StateEnum,
    allowed: &'static [StateEnum],
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
