#![feature(assert_matches)]
use std::assert_matches::assert_matches;

use state_machine::StateMachine;

#[derive(Debug, PartialEq, Eq, Clone, Copy, StateMachine)]
#[state_machine(error = "TooSoon")]
enum Semaphore {
    #[state(to(Green=turn_on))]
    Off(()),
    #[state(try_to(Green = rg), to(Off = turn_off))]
    Red(u8),
    #[state(try_to(Red = yr), to(Off = turn_off))]
    Yellow(u8),
    #[state(try_to(Yellow = gy), to(Off = turn_off))]
    Green(u8),
}

#[derive(Debug, PartialEq, Eq)]
struct TooSoon {
    from: SemaphoreState,
    to: SemaphoreState,
    remaining: u8,
}

fn gy(time: &mut u8) -> Result<u8, TooSoon> {
    if *time == 0 {
        Ok(5)
    } else {
        Err(TooSoon {
            from: SemaphoreState::Green,
            to: SemaphoreState::Yellow,
            remaining: *time,
        })
    }
}

fn yr(time: &mut u8) -> Result<u8, TooSoon> {
    if *time == 0 {
        Ok(30)
    } else {
        Err(TooSoon {
            from: SemaphoreState::Yellow,
            to: SemaphoreState::Red,
            remaining: *time,
        })
    }
}

fn rg(time: &mut u8) -> Result<u8, TooSoon> {
    if *time == 0 {
        Ok(30)
    } else {
        Err(TooSoon {
            from: SemaphoreState::Red,
            to: SemaphoreState::Green,
            remaining: *time,
        })
    }
}

fn turn_on(_unit: &mut ()) -> u8 {
    30
}
fn turn_off(_time: &mut u8) -> () {}

#[test]
fn state() {
    let sem = Semaphore::Red(10);
    assert_eq!(sem.state(), SemaphoreState::Red)
}
#[test]
fn failed_transition() {
    let mut sem = Semaphore::Red(10);
    assert_matches!(sem.try_from_red_to_green(), Ok(Err(TooSoon { .. })));
    assert_matches!(
        sem.try_from_yellow_to_red(),
        Err(SemaphoreWrongState {
            method: "try_from_yellow_to_red",
            valid: &[SemaphoreState::Yellow],
            found: SemaphoreState::Red
        })
    );
    assert_matches!(
        sem.state_try_to_yellow(),
        Err(SemaphoreWrongState {
            method: "state_try_to_yellow",
            valid: &[SemaphoreState::Green, SemaphoreState::Yellow],
            found: SemaphoreState::Red
        })
    )
}

#[test]
fn test_run() {
    let mut sem = Semaphore::Off(());
    sem.from_off_to_off().unwrap();
    sem.state_to_green().unwrap();
    assert!(sem.is_green());
    assert_eq!(sem.as_green(), Ok(&30));
    *sem.as_green_mut().unwrap() = 0;
    sem.state_try_to_yellow().unwrap().unwrap();
    *sem.as_yellow_mut().unwrap() = 2;
    assert_eq!(sem.state(), SemaphoreState::Yellow);
    assert_matches!(sem.try_from_yellow_to_red(), Ok(Err(_)));
    assert_eq!(sem.as_yellow(), Ok(&2));
    sem.state_to_off().unwrap();
}

#[test]
fn infallible_as_fallible() {
    let mut sem = Semaphore::Off(());
    sem.from_off_to_off().unwrap();
    assert!(sem.is_off());
    sem.try_from_off_to_off().unwrap().unwrap();
    assert!(sem.is_off());
    sem.state_to_off().unwrap();
    assert!(sem.is_off());
    sem.state_try_to_off().unwrap().unwrap();
    assert!(sem.is_off());
}
