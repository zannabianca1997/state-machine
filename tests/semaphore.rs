#![feature(assert_matches)]
use std::assert_matches::assert_matches;

use state_machine::StateMachine;

#[derive(Debug, PartialEq, Eq, Clone, Copy, StateMachine)]
#[state_machine(error = "TooSoon")]
enum Semaphore {
    #[state(try_to(Green = rg))]
    Red(u8),
    #[state(try_to(Red = yr))]
    Yellow(u8),
    #[state(try_to(Yellow = gy))]
    Green(u8),
}

#[derive(Debug, PartialEq, Eq)]
struct TooSoon {
    from: SemaphoreState,
    to: SemaphoreState,
    remaining: u8,
}

fn gy(time: u8) -> Result<u8, (u8, TooSoon)> {
    if time == 0 {
        Ok(5)
    } else {
        Err((
            time,
            TooSoon {
                from: SemaphoreState::Green,
                to: SemaphoreState::Yellow,
                remaining: time,
            },
        ))
    }
}

fn yr(time: u8) -> Result<u8, (u8, TooSoon)> {
    if time == 0 {
        Ok(30)
    } else {
        Err((
            time,
            TooSoon {
                from: SemaphoreState::Yellow,
                to: SemaphoreState::Red,
                remaining: time,
            },
        ))
    }
}

fn rg(time: u8) -> Result<u8, (u8, TooSoon)> {
    if time == 0 {
        Ok(30)
    } else {
        Err((
            time,
            TooSoon {
                from: SemaphoreState::Red,
                to: SemaphoreState::Green,
                remaining: time,
            },
        ))
    }
}

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
    )
}
