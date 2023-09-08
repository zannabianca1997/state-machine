#![feature(return_position_impl_trait_in_trait)]
use state_machine::{TransitionTo, TryTransitionTo};
enum WaterFiller {
    Off(Option<Bottle>),
    Filling(Filling),
}
trait WaterFillerSM {
    fn from_filling_to_filling(&mut self) -> ::std::option::Option<()>;
    fn from_filling_to_off(&mut self) -> ::std::option::Option<()>;
    fn try_from_filling_to_filling(
        &mut self,
    ) -> ::std::option::Option<
        ::std::result::Result<(), <Filling as ::state_machine::TryTransitionTo<Filling>>::Error>,
    >;
    fn try_from_filling_to_off(
        &mut self,
    ) -> ::std::option::Option<
        ::std::result::Result<
            (),
            <Filling as ::state_machine::TryTransitionTo<Option<Bottle>>>::Error,
        >,
    >;
    fn from_off_to_off(&mut self) -> ::std::option::Option<()>;
    fn try_from_off_to_filling(
        &mut self,
    ) -> ::std::option::Option<
        ::std::result::Result<
            (),
            <Option<Bottle> as ::state_machine::TryTransitionTo<Filling>>::Error,
        >,
    >;
    fn try_from_off_to_off(
        &mut self,
    ) -> ::std::option::Option<
        ::std::result::Result<
            (),
            <Option<Bottle> as ::state_machine::TryTransitionTo<Option<Bottle>>>::Error,
        >,
    >;
}
impl WaterFillerSM for WaterFiller {
    fn from_filling_to_filling(&mut self) -> ::std::option::Option<()> {
        let mut res = ::std::option::Option::None;
        ::state_machine::take_mut::take(self, |this| {
            if let WaterFiller::Filling(content) = this {
                res = ::std::option::Option::Some(());
                WaterFiller::Filling(::state_machine::TransitionTo::transition(content))
            } else {
                res = ::std::option::Option::None;
                this
            }
        });
        res
    }
    fn from_filling_to_off(&mut self) -> ::std::option::Option<()> {
        let mut res = ::std::option::Option::None;
        ::state_machine::take_mut::take(self, |this| {
            if let WaterFiller::Filling(content) = this {
                res = ::std::option::Option::Some(());
                WaterFiller::Off(::state_machine::TransitionTo::transition(content))
            } else {
                res = ::std::option::Option::None;
                this
            }
        });
        res
    }
    fn try_from_filling_to_filling(
        &mut self,
    ) -> ::std::option::Option<
        ::std::result::Result<(), <Filling as ::state_machine::TryTransitionTo<Filling>>::Error>,
    > {
        let mut res = ::std::option::Option::None;
        ::state_machine::take_mut::take(self, |this| {
            if let WaterFiller::Filling(content) = this {
                match ::state_machine::TryTransitionTo::try_transition(&content) {
                    ::std::result::Result::Ok(f) => {
                        res = ::std::option::Option::Some(::std::result::Result::Ok(()));
                        WaterFiller::Filling(f(content))
                    }
                    ::std::result::Result::Err(err) => {
                        res = ::std::option::Option::Some(::std::result::Result::Err(err));
                        WaterFiller::Filling(content)
                    }
                }
            } else {
                res = ::std::option::Option::None;
                this
            }
        });
        res
    }
    fn try_from_filling_to_off(
        &mut self,
    ) -> ::std::option::Option<
        ::std::result::Result<
            (),
            <Filling as ::state_machine::TryTransitionTo<Option<Bottle>>>::Error,
        >,
    > {
        let mut res = ::std::option::Option::None;
        ::state_machine::take_mut::take(self, |this| {
            if let WaterFiller::Filling(content) = this {
                match ::state_machine::TryTransitionTo::try_transition(&content) {
                    ::std::result::Result::Ok(f) => {
                        res = ::std::option::Option::Some(::std::result::Result::Ok(()));
                        WaterFiller::Off(f(content))
                    }
                    ::std::result::Result::Err(err) => {
                        res = ::std::option::Option::Some(::std::result::Result::Err(err));
                        WaterFiller::Filling(content)
                    }
                }
            } else {
                res = ::std::option::Option::None;
                this
            }
        });
        res
    }
    fn from_off_to_off(&mut self) -> ::std::option::Option<()> {
        let mut res = ::std::option::Option::None;
        ::state_machine::take_mut::take(self, |this| {
            if let WaterFiller::Off(content) = this {
                res = ::std::option::Option::Some(());
                WaterFiller::Off(::state_machine::TransitionTo::transition(content))
            } else {
                res = ::std::option::Option::None;
                this
            }
        });
        res
    }
    fn try_from_off_to_filling(
        &mut self,
    ) -> ::std::option::Option<
        ::std::result::Result<
            (),
            <Option<Bottle> as ::state_machine::TryTransitionTo<Filling>>::Error,
        >,
    > {
        let mut res = ::std::option::Option::None;
        ::state_machine::take_mut::take(self, |this| {
            if let WaterFiller::Off(content) = this {
                match ::state_machine::TryTransitionTo::try_transition(&content) {
                    ::std::result::Result::Ok(f) => {
                        res = ::std::option::Option::Some(::std::result::Result::Ok(()));
                        let content = f(content);
                        WaterFiller::Filling(content)
                    }
                    ::std::result::Result::Err(err) => {
                        res = ::std::option::Option::Some(::std::result::Result::Err(err));
                        WaterFiller::Off(content)
                    }
                }
            } else {
                res = ::std::option::Option::None;
                this
            }
        });
        res
    }
    fn try_from_off_to_off(
        &mut self,
    ) -> ::std::option::Option<
        ::std::result::Result<
            (),
            <Option<Bottle> as ::state_machine::TryTransitionTo<Option<Bottle>>>::Error,
        >,
    > {
        let mut res = ::std::option::Option::None;
        ::state_machine::take_mut::take(self, |this| {
            if let WaterFiller::Off(content) = this {
                match ::state_machine::TryTransitionTo::try_transition(&content) {
                    ::std::result::Result::Ok(f) => {
                        res = ::std::option::Option::Some(::std::result::Result::Ok(()));
                        let content = f(content);
                        WaterFiller::Off(content)
                    }
                    ::std::result::Result::Err(err) => {
                        res = ::std::option::Option::Some(::std::result::Result::Err(err));
                        WaterFiller::Off(content)
                    }
                }
            } else {
                res = ::std::option::Option::None;
                this
            }
        });
        res
    }
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
        todo!()
    }
}
