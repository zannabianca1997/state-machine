#![feature(return_position_impl_trait_in_trait)]
use state_machine::{StateMachine, TransitionTo, TryTransitionTo};

#[derive(StateMachine)]
enum WaterFiller {
    #[state(try_to(Filling))]
    Off(Option<Bottle>),
    #[state(to(Off))]
    Filling(Filling),
}

struct Bottle {
    content: u8,
    capacity: u8,
}

struct MissingBottle;

impl TryTransitionTo<Filling> for Option<Bottle> {
    type Error = MissingBottle;

    fn try_transition(&self) -> Result<impl FnOnce(Self) -> Filling, Self::Error>
    where
        Self: Sized,
    {
        if self.is_some() {
            Ok(|x: Self| Filling(x.unwrap()))
        } else {
            Err(MissingBottle)
        }
    }
}

struct Filling(Bottle);

impl TransitionTo<Option<Bottle>> for Filling {
    fn transition(self) -> Option<Bottle> {
        Some(self.0)
    }
}

#[test]
fn cycle() {
    let mut filler = WaterFiller::Off(None);
    filler.from_off_to_off().unwrap();
    filler.try_from_off_to_off().unwrap().unwrap();
}
