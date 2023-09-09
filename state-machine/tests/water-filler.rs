#![feature(return_position_impl_trait_in_trait)]
#![feature(never_type)]
use state_machine::{StateMachine, TransitionTo, TryTransitionTo};

#[derive(StateMachine)]
#[state_machine(error = "MissingBottle")]
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

#[derive(Debug)]
struct MissingBottle;
impl From<!> for MissingBottle {
    fn from(value: !) -> Self {
        value
    }
}

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
impl Filling {
    fn fill(&mut self) {
        self.0.content = self.0.capacity;
    }
}

impl TransitionTo<Option<Bottle>> for Filling {
    fn transition(self) -> Option<Bottle> {
        Some(self.0)
    }
}

#[test]
fn cycle() {
    let mut filler = WaterFiller::Off(None);
    assert!(filler.is_off());
    *filler.as_off_mut().unwrap() = Some(Bottle {
        content: 0,
        capacity: 8,
    });
    filler.try_from_off_to_filling().unwrap().unwrap();
    assert_eq!(filler.state(), WaterFillerState::Filling);
    filler.as_filling_mut().unwrap().fill();
    filler.from_filling_to_off().unwrap();
    let bottle = filler.as_off_mut().unwrap().take().unwrap();
    assert_eq!(bottle.capacity, bottle.content)
}

#[test]
fn cycle_with_global_transitions() {
    let mut filler = WaterFiller::Off(None);
    assert!(filler.is_off());
    *filler.as_off_mut().unwrap() = Some(Bottle {
        content: 0,
        capacity: 8,
    });
    filler.try_to_filling().unwrap().unwrap();
    assert_eq!(filler.state(), WaterFillerState::Filling);
    filler.as_filling_mut().unwrap().fill();
    filler.to_off().unwrap();
    let bottle = filler.as_off_mut().unwrap().take().unwrap();
    assert_eq!(bottle.capacity, bottle.content)
}

#[test]
fn cycle_with_generic_transitions() {
    let mut filler = WaterFiller::Off(None);
    assert!(filler.is_off());
    *filler.as_off_mut().unwrap() = Some(Bottle {
        content: 0,
        capacity: 8,
    });
    filler.try_to(WaterFillerState::Filling).unwrap().unwrap();
    assert_eq!(filler.state(), WaterFillerState::Filling);
    filler.as_filling_mut().unwrap().fill();
    filler.to(WaterFillerState::Off).unwrap();
    let bottle = filler.as_off_mut().unwrap().take().unwrap();
    assert_eq!(bottle.capacity, bottle.content)
}
