use parser::ParserResult;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Display, Formatter};
use std::fs;
use std::hash::{Hash, Hasher};
pub mod parser;

#[cfg(test)]
mod test;

/// A "set" of dependencies, stored in a map. For `A` see `Task::after`.
pub type Dependencies<A> = HashMap<String, A>;

/// Details of a task, as parsed. For `A` see `Task::after`.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Task<A> {
    // Implementation comment: name field could be omitted if we pass a
    // HashMap<String, Task> everywhere, but that would be error prone/coupled
    // and not easily testable.
    name: String,
    duration: usize,
    /// Keys are name(s) of task(s) that this task (self) depends on. If `A` is
    /// `(usize, usize)`, it indicates a row and column (both starting from 0)
    /// where that dependency name was read from. That's used for validation
    /// (and errors). Otherwise `A` is the void/unit type `()`.
    after: Dependencies<A>,
}

impl<A> Task<A> {
    pub fn new(name: impl Into<String>, duration: usize, after: Dependencies<A>) -> Self {
        Self {
            name: name.into(),
            duration,
            after,
        }
    }
}

/// Without this, `Task` couldn't contain a `HashMap` (which doesn't implement
/// `Hash`).
#[allow(clippy::derive_hash_xor_eq)]
impl<A> Hash for Task<A> {
    fn hash<H: Hasher>(&self, mut state: &mut H) {
        self.name.hash(&mut state)
    }
}

/// Map of task name => Task.
type TasksMapWithDependencyInfo<A> = HashMap<String, Task<A>>;

type TasksMapNotValidated = TasksMapWithDependencyInfo<(usize, usize)>;
type TasksMapValidated = TasksMapWithDependencyInfo<()>;

/// Map: task name => hash set of reference(s) to its dependent `Task`
/// instance(s). It's a reverse relation to `Task`'s `after` field. If a task
/// has no dependents, the value (vector) will still be present and empty.
pub type DependentsMap<'a> = HashMap<String, HashSet<&'a Task<()>>>;

/// Collect dependents of each task.
pub fn direct_dependents(tasks: &TasksMapValidated) -> DependentsMap {
    let mut with_all_sets = DependentsMap::with_capacity(tasks.len());
    tasks.iter().for_each(|(name, _task)| {
        with_all_sets.insert(name.clone(), HashSet::new());
    });
    tasks
        .iter()
        .fold(with_all_sets, |mut result, (_name, task)| {
            task.after.iter().for_each(|prerequisite_name| {
                let dependents = result.get_mut(prerequisite_name.0);
                assert!(dependents.is_some()); // was populated in with_all_sets
                let dependents = dependents.unwrap();
                debug_assert!(!dependents.contains(&task));
                dependents.insert(task);
            });
            result
        })
}

/// Represents an active or completed path (task). Containing: 1. the name(s) of
/// tasks on the path, starting from the very first, and ending with the current
/// (or last) task, and 2. finalization time point (not duration) of the whole
/// path.
type Path = (Vec<String>, usize);

#[derive(Debug, PartialEq)]
pub struct TasksResult {
    critical_path: Option<Path>,
    running_time: usize,
    /// Max. number of parallel active paths at any point.
    parallelism: usize,
}

#[allow(clippy::new_without_default)]
impl TasksResult {
    pub fn new() -> Self {
        Self {
            critical_path: None,
            running_time: 0,
            parallelism: 0,
        }
    }
}

impl Display for TasksResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let critical_path_formatted = match self.critical_path.clone() {
            None => "".to_owned(),
            Some((path, _finalization_point)) => path.join("->"),
        };
        writeln!(f, "Critical: {}", critical_path_formatted)?;
        writeln!(f, "Minimum: {}", self.running_time)?;
        write!(f, "Parallelism: {}", self.parallelism)
    }
}

struct TasksIteration {
    /// Map: target/destination task => its active Path (one of possibly
    /// multiple parallel active paths).
    active_paths: HashMap<String, Path>,
    // Map: completed task name => time of completion (total duration from the
    // very start of any tasks, that is from time 0, until the end of this
    // task).
    completed_tasks: HashMap<String, usize>,
    /// Name(s) of task(s) not started yet.
    tasks_not_started_yet: HashSet<String>,
    result: TasksResult,
}

impl TasksIteration {
    pub fn new() -> Self {
        Self {
            active_paths: HashMap::new(),
            completed_tasks: HashMap::new(),
            tasks_not_started_yet: HashSet::new(),
            result: TasksResult::new(),
        }
    }
}

pub fn process_file_path(file_path: &str) -> ParserResult<TasksResult> {
    let input_text = fs::read_to_string(file_path).expect("Can't read the input file.");
    parse_and_process(&input_text)
}

pub fn parse_and_process(input_text: &str) -> ParserResult<TasksResult> {
    let parsed_tasks_result = parser::parse_input_and_validate(input_text);

    match parsed_tasks_result {
        Err(e) => Err(e),
        Ok(tasks_map) => {
            let direct_dependents = direct_dependents(&tasks_map);

            let result = process_tasks(&tasks_map, &direct_dependents);
            Ok(result)
        }
    }
}

pub fn process_tasks(
    tasks_map: &TasksMapValidated,
    direct_dependents: &DependentsMap,
) -> TasksResult {
    let (mut state, mut stop_time_point) = initial_state_and_stop_time_point(tasks_map);

    while state.completed_tasks.len() < tasks_map.len() {
        debug_assert!(state.result.running_time < stop_time_point);
        state.result.running_time = stop_time_point;

        let just_completed_path_entries =
            collect_just_completed_path_entries(&state.active_paths, stop_time_point);
        register_completed_tasks(
            &mut state.completed_tasks,
            just_completed_path_entries.clone(),
            state.result.running_time,
        );

        let (new_active_paths, new_critical_path) = collect_new_active_paths_and_critical_path(
            &state,
            just_completed_path_entries.clone(),
            direct_dependents,
        );
        state.result.critical_path = new_critical_path;

        // Remap to owned (cloned), so that we can manipulate them later.
        let just_completed_path_entries = path_entries_to_owned(just_completed_path_entries);

        state.active_paths.extend(new_active_paths);

        state
            .active_paths
            .retain(|target_task, _path| just_completed_path_entries.get(target_task).is_none());

        state.result.parallelism = state.result.parallelism.max(state.active_paths.len());

        let next_stop_time_point = state.active_paths.iter().fold(
            usize::MAX,
            |finalization_found, (_target_task, (_chain, chain_finalization))| {
                finalization_found.min(*chain_finalization)
            },
        );
        debug_assert!(next_stop_time_point > stop_time_point);
        stop_time_point = next_stop_time_point;
    }
    state.result
}

/// Return (a tuple of) the initial state (blank `TasksIteration` with initial
/// `tasks_not_started_yet` and `active_paths`) and the first stop time point
/// (based on the shortest root task put into `active_paths`).
fn initial_state_and_stop_time_point(tasks_map: &TasksMapValidated) -> (TasksIteration, usize) {
    let mut state = TasksIteration::new();
    state.tasks_not_started_yet = tasks_map
        .iter()
        .filter(|&(_name, task)| !task.after.is_empty())
        .map(|(name, _)| name)
        .cloned()
        .collect::<HashSet<_>>();

    let root_tasks = tasks_map
        .iter()
        .filter(|&(_name, task)| task.after.is_empty())
        .map(|(_, task)| task)
        .collect::<HashSet<_>>();

    root_tasks.iter().for_each(|&root| {
        state
            .active_paths
            .insert(root.name.clone(), (vec![root.name.clone()], root.duration));
    });
    let stop_time_point = root_tasks
        .iter()
        .fold(usize::MAX, |step, &root| step.min(root.duration));
    (state, stop_time_point)
}

fn collect_just_completed_path_entries(
    active_paths: &HashMap<String, Path>,
    stop_time_point: usize,
) -> impl Iterator<Item = (&String, &Path)> + Clone {
    active_paths
        .iter()
        .filter(move |(_target_task, &(_, finalization))| {
            // step_size is the time until the nearest finalization, so nothing
            // can complete earlier:
            debug_assert!(finalization >= stop_time_point);
            finalization == stop_time_point
        })
}

fn register_completed_tasks<'a>(
    completed_tasks: &mut HashMap<String, usize>,
    just_completed_path_entries: impl Iterator<Item = (&'a String, &'a Path)>,
    running_time: usize,
) {
    completed_tasks.extend(
        just_completed_path_entries.map(|(target_task_name, (_task_name_chain, _))| {
            (target_task_name.clone(), running_time)
        }),
    );
}

fn collect_new_active_paths_and_critical_path<'a>(
    state: &TasksIteration,
    just_completed_path_entries: impl Iterator<Item = (&'a String, &'a Path)>,
    direct_dependents: &DependentsMap,
) -> (HashMap<String, Path>, Option<Path>) {
    let mut new_active_paths = <HashMap<String, Path>>::new();
    let mut critical_path = state.result.critical_path.clone();

    for (target_task_name, (task_name_chain, finalization)) in just_completed_path_entries {
        debug_assert_eq!(*finalization, state.result.running_time);

        let prerequisite = target_task_name;

        for &dependent in &direct_dependents[prerequisite] {
            if dependent
                .after
                .iter()
                .all(|other_prerequisite| state.completed_tasks.contains_key(other_prerequisite.0))
            {
                let mut new_chain = task_name_chain.clone();
                new_chain.push(dependent.name.clone());
                let new_path = (new_chain, state.result.running_time + dependent.duration);
                new_active_paths.insert(dependent.name.clone(), new_path);
            }
        }

        critical_path = Some((task_name_chain.clone(), *finalization));
    }
    (new_active_paths, critical_path)
}

fn path_entries_to_owned<'a>(
    paths: impl Iterator<Item = (&'a String, &'a Path)>,
) -> HashMap<String, Path> {
    paths
        .map(|(target_task, path)| (target_task.clone(), path.clone()))
        .collect::<HashMap<_, _>>()
}
