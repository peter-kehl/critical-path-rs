use crate::test::to;
use crate::{Dependencies, Task, TasksMapValidated};

// Tests are in reverse historical/gradual order - the basic ones are at the
// bottom.

fn prerequisites_to_map(prerequisites: Vec<&str>) -> Dependencies<()> {
    prerequisites.into_iter().map(|s| (to(s), ())).collect()
}

#[test]
fn test_simple_input() {
    let result = crate::parser::parse_and_validate_file_path("test/multiple_critical_paths.in.txt");
    assert!(result.is_ok());

    let mut expected = TasksMapValidated::new();
    expected.insert(
        to("Water"),
        Task::new("Water", 1, prerequisites_to_map(vec![])),
    );
    expected.insert(
        to("Yeast"),
        Task::new("Yeast", 2, prerequisites_to_map(vec!["Water"])),
    );
    expected.insert(
        to("Flour"),
        Task::new("Flour", 1, prerequisites_to_map(vec!["Yeast"])),
    );
    expected.insert(
        to("Oil"),
        Task::new("Oil", 1, prerequisites_to_map(vec!["Flour"])),
    );
    expected.insert(
        to("Knead"),
        Task::new("Knead", 5, prerequisites_to_map(vec!["Oil"])),
    );
    expected.insert(
        to("Rise"),
        Task::new("Rise", 120, prerequisites_to_map(vec!["Knead"])),
    );
    expected.insert(
        to("Bake_pizza"),
        Task::new("Bake_pizza", 10, prerequisites_to_map(vec!["Rise"])),
    );
    expected.insert(
        to("Amino_powder"),
        Task::new("Amino_powder", 1, prerequisites_to_map(vec!["Water"])),
    );
    expected.insert(
        to("Shake"),
        Task::new("Shake", 1, prerequisites_to_map(vec!["Amino_powder"])),
    );
    expected.insert(
        to("Drink"),
        Task::new("Drink", 1, prerequisites_to_map(vec!["Shake"])),
    );
    expected.insert(
        to("Nuts"),
        Task::new("Nuts", 1, prerequisites_to_map(vec![])),
    );
    expected.insert(
        to("Jacket"),
        Task::new("Jacket", 1, prerequisites_to_map(vec![])),
    );
    expected.insert(
        to("Charge_batteries"),
        Task::new("Charge_batteries", 120, prerequisites_to_map(vec![])),
    );
    expected.insert(
        to("Pack"),
        Task::new(
            "Pack",
            15,
            prerequisites_to_map(vec!["Water", "Nuts", "Charge_batteries", "Jacket"]),
        ),
    );
    expected.insert(
        to("Ready_for_a_hike"),
        Task::new("Ready_for_a_hike", 5, prerequisites_to_map(vec!["Pack"])),
    );
    expected.insert(
        to("Call_friends"),
        Task::new("Call_friends", 3, prerequisites_to_map(vec![])),
    );
    expected.insert(
        to("Laundry"),
        Task::new("Laundry", 120, prerequisites_to_map(vec!["Call_friends"])),
    );
    expected.insert(
        to("Get_ready"),
        Task::new("Get_ready", 2, prerequisites_to_map(vec!["Laundry"])),
    );
    expected.insert(
        to("Go_to_party"),
        Task::new("Go_to_party", 15, prerequisites_to_map(vec!["Get_ready"])),
    );
    assert_eq!(result.unwrap(), expected);
}

const AFER_TYPO: &str = r#"A(1)
B(1) afer [A]"#;

#[test]
fn test_afer_typo() {
    let result = crate::parser::parse_input(AFER_TYPO);
    assert!(result.is_err());
    assert_eq!(
        format!("{}", result.err().unwrap()),
        "Error: line 2, column 6"
    );
}

#[test]
fn test_negative_duration() {
    let result = crate::parser::parse_input("A(-1)");
    assert!(result.is_err());
    assert_eq!(
        format!("{}", result.err().unwrap()),
        "Error: line 1, column 3"
    );
}

#[test]
fn test_peek_non_task_name_matched_some_input() {
    let matched = crate::parser::peek_non_task_name(&mut "!".char_indices().peekable());
    assert_eq!(matched, (true, Some(0)));
}

#[test]
fn test_next_non_whitespace_found_whitespace() {
    let matched = crate::parser::peek_non_whitespace(&mut "  a".char_indices().peekable());
    assert_eq!(matched, (true, Some(2)));
}

#[test]
fn peek_not_matched_some_input() {
    let zero_match = crate::parser::peek(|_| false, &mut "_".char_indices().peekable());
    assert_eq!(zero_match, (false, Some(0)));
}
#[test]
fn peek_matched_no_input() {
    let zero_match = crate::parser::peek(|_| true, &mut "".char_indices().peekable());
    assert_eq!(zero_match, (true, None));
}
#[test]
fn peek_matched_found() {
    let zero_match = crate::parser::peek(|_| true, &mut "_".char_indices().peekable());
    assert_eq!(zero_match, (true, Some(0)));
}
