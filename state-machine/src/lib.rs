#![feature(return_position_impl_trait_in_trait)]

use std::convert::Infallible;

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
