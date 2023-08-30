use state_machine::StateMachine;

/// This should compile fine
#[derive(StateMachine)]
#[allow(dead_code)]
enum SM {}
