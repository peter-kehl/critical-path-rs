mod parser;

use crate::parser::ParserError;
use crate::DependentsMap;
use core::hash::Hash;
use std::collections::HashSet;

pub fn to(s: &str) -> String {
    s.to_owned()
}

// Tests are in reverse historical/gradual order - the basic ones are at the
// bottom.

#[test]
fn test_non_existing_dependency() {
    let result = crate::process_file_path("test/non-existing_dependency.in");
    assert_eq!(result, Err(ParserError::new(2, 9)));
}

#[test]
fn test_dependency_without_open_bracket() {
    let result = crate::process_file_path("test/dependency_without_open_bracket.in");
    assert_eq!(result, Err(ParserError::new(3, 9)));
}

fn owned(slices: Vec<&str>) -> Vec<String> {
    slices.iter().map(|&s| to(s)).collect()
}

#[test]
fn test_process_tasks() {
    let result = crate::parser::parse_and_validate_file_path("test/multiple_critical_paths.in.txt");
    assert!(result.is_ok());

    let tasks_map = result.unwrap();
    let direct_dependents = crate::direct_dependents(&tasks_map);

    let result = crate::process_tasks(&tasks_map, &direct_dependents);
    println!("process_tasks() -> {}", result);

    assert_eq!(result.running_time, 140);
    assert_eq!(result.parallelism, 4);

    let (critical_chain, running_time) = result.critical_path.unwrap();
    assert_eq!(result.running_time, running_time);

    assert!(vec![
        owned(vec![
            "Water",
            "Yeast",
            "Flour",
            "Oil",
            "Knead",
            "Rise",
            "Bake_pizza"
        ]),
        owned(vec!["Call_friends", "Laundry", "Get_ready", "Go_to_party"]),
        owned(vec!["Charge_batteries", "Pack", "Ready_for_a_hike"])
    ]
    .contains(&critical_chain));
}

// This could be done with a macro, but no need.
fn vec_to_hashset<T>(v: &Vec<T>) -> HashSet<T>
where
    T: Eq + Clone + Hash,
{
    v.iter().cloned().collect::<HashSet<_>>()
}

#[test]
fn test_simple_direct_dependents() {
    let result = crate::parser::parse_and_validate_file_path("test/multiple_critical_paths.in.txt");
    assert!(result.is_ok());
    let tasks_map = result.unwrap();
    let result_dependents = crate::direct_dependents(&tasks_map);

    let mut expected = DependentsMap::new();
    expected.insert(
        to("Water"),
        vec_to_hashset(&vec![
            tasks_map.get(&to("Yeast")).unwrap(),
            tasks_map.get(&to("Pack")).unwrap(),
            tasks_map.get(&to("Amino_powder")).unwrap(),
        ]),
    );
    expected.insert(
        to("Yeast"),
        vec_to_hashset(&(vec![tasks_map.get(&to("Flour")).unwrap()])),
    );
    expected.insert(
        "Flour".to_owned(),
        vec_to_hashset(&(vec![tasks_map.get(&to("Oil")).unwrap()])),
    );
    expected.insert(
        "Oil".to_owned(),
        vec_to_hashset(&(vec![tasks_map.get(&to("Knead")).unwrap()])),
    );
    expected.insert(
        "Knead".to_owned(),
        vec_to_hashset(&(vec![tasks_map.get(&to("Rise")).unwrap()])),
    );
    expected.insert(
        to("Rise"),
        vec_to_hashset(&(vec![tasks_map.get(&to("Bake_pizza")).unwrap()])),
    );
    expected.insert(to("Bake_pizza"), HashSet::new());

    expected.insert(
        "Amino_powder".to_owned(),
        vec_to_hashset(&(vec![tasks_map.get(&to("Shake")).unwrap()])),
    );
    expected.insert(
        "Shake".to_owned(),
        vec_to_hashset(&(vec![tasks_map.get(&to("Drink")).unwrap()])),
    );
    expected.insert(to("Drink"), HashSet::new());

    expected.insert(
        "Nuts".to_owned(),
        vec_to_hashset(&(vec![tasks_map.get(&to("Pack")).unwrap()])),
    );
    expected.insert(
        "Jacket".to_owned(),
        vec_to_hashset(&(vec![tasks_map.get(&to("Pack")).unwrap()])),
    );
    expected.insert(
        "Charge_batteries".to_owned(),
        vec_to_hashset(&(vec![tasks_map.get(&to("Pack")).unwrap()])),
    );
    expected.insert(
        "Pack".to_owned(),
        vec_to_hashset(&(vec![tasks_map.get(&to("Ready_for_a_hike")).unwrap()])),
    );
    expected.insert(to("Ready_for_a_hike"), HashSet::new());

    expected.insert(
        "Call_friends".to_owned(),
        vec_to_hashset(&(vec![tasks_map.get(&to("Laundry")).unwrap()])),
    );
    expected.insert(
        "Laundry".to_owned(),
        vec_to_hashset(&(vec![tasks_map.get(&to("Get_ready")).unwrap()])),
    );
    expected.insert(
        "Get_ready".to_owned(),
        vec_to_hashset(&(vec![tasks_map.get(&to("Go_to_party")).unwrap()])),
    );
    expected.insert(to("Go_to_party"), HashSet::new());

    println!("result dependents: {:#?}", result_dependents);
    assert_eq!(result_dependents, expected);
}

#[test]
fn test_error_formatting() {
    let error = ParserError::new(1, 3);
    assert_eq!(error.to_string(), "Error: line 1, column 3");
}
