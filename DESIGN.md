# Toolchain
No initial need for any unstable features, so current stable Rust (1.61.0+) with
standard toolchain (`cargo`). Chances are this would work with much older
versions of Rust, too. (We would need `nightly` if we used `cargo bench` and
`#[bench]`, but the code should be forward compatible.)

# Error handling after parsing
Given the clear specification and the guarantee that tasks are not cyclic, there
should not be any expectable/recoverable error after parsing. Hence, if we have
any extra assertions, they can be `panic!`, `assert!` or `dbg_assert!`. (Plus,
it would be quite impossible to relate any error back to the input location,
anyway.)

Input-related errors have line numbers and column numbers starting from 1. To
keep this simple, column numbers are reported as ASCII/byte-based, not Unicode
character-based.

However, to prevent sanity while developing, and to prevent CI/CD costs and
delays, we can optionally add checks that the tasks are not cyclic.

# Implementation
This design uses term "task" for either `Task` instance/type or task name
interchangeably, unless specified otherwise.

## Data types
### Use tuples and type keyword
Using tuples and `type` keyword, rather than creating `struct`-s for everything
possible, allow for cheap abstractions and fluid/refactoring-friendly code. The
cost is a potential for logical errors, for example by mixing up active and
completed `Path` instances. That's OK given the size of this assignment.

### Use String, at least initially
Even though we know that tasks are not cyclic, `rustc` doesn't know that. We
can't have `Task` instances double-linked (that a `Task` would refer to both its
prerequisites (`after`) and its direct dependents as instances of the same type `Task`)
in safe Rust. (We could use `RefCell` but that would complicate the code, and it
wouldn't be thread-safe if we ever want to paralellize this.)

Even if we had only one of the task relationships (`after` or `direct_dependents`)
stored through `Task` references, we'd need lifetimes. Doing so would guarantee
that (one-way) relationship as consistent: the instances referred to would be of
type `Task`, and if we don't have any `Task`(s) stored anywhere else (other than
our only collection of them: `TasksMap`), then all the (referred-to) `Task`
instances would be healthy.

However, that would complicate the code (when updating/replacing `Task`
instances while processing the input). We'd need to tie the lifetime of Task
instances with a collection where we store them. We don't use string slices
(`&str`) for the same reason - at least initially. So both types of `Task`
relationships (`after` and `direct_dependents`) are done by `String` names of the
referred `Task`(s), and to get the referred `Task` instance we fetch it by its
name from a collection (`TasksMap`).

Using `String` instead of string slice (`&str`) means cloning it - that's the
price for simplicity.

### Task
A struct reflecting the input data: `name`, `duration`, `after`.

### TasksMap
A `HashMap` that stores all `Task`-s, keyed by their names. We have one (single)
`TasksMap` instance. Whenever we refer to a task by name, we fetch it from this
map.

### DependentsMap
A `HashMap` of direct "depended-by" relationship. It maps a task name to a `HashSet` of
`Task` references that directly depend on that task (key). The values in this
map are an exception to using task names for relationships, as this saves us a
trip through `TasksMap` on every iteration (below).
(The values in the map could be `Vec` of `Task` references, but but that would make testing non-deterministic...)

### Path
A tuple of
 1. a `Vec`tor of `String`-s that are all the tasks on this path (so far),
    starting with the very first task, ending with the current task (if still
    active) or last task (if completed), and 
 2. `usize` finalization time point (not leftover duration) of the current (or
    last added) task on this path.

### TaskParserStage
An enum representing state when parsing the input, specifically: which part of a
task input is expected at the current input position (the position itself is not
a part of `TaskParserStage`). It doesn't contain any already collected data of
the task instance being parsed to/created (from the current input position).
Possible variants (but we may need only some): `BeforeTask, OpenParen, Duration,
CloseParen, AfterDuration, OpenBracket, BeforePrerequisite, CloseBracket`.

### ParserState
A struct, holding an `TaskParserStage` instance, a `ParsedTaskInfo` instance,
and possibly a few house-keeping fields.

### ParsedTaskInfo
A `struct` (or a tuple) representing any data/fields of a task collected so far.
Containing three fields, all are an `Option`: `name: Option<String>, duration:
Option<usize>, after: Vec<String>`.

### TasksIteration
Describes the state of a state machine as we process tasks and their paths in a
loop described below. A struct with fields:
 1. `active_paths`: `HashMap` of current target/destination task name mapped to its `Path`. This can't be a simple `Vec` or `HashSet` of `Path`, because the same target task may have several paths leading to it. That would make calculating `parallelism` harder. (The choice of the active `Path`, out of all possible parallel active paths to the same target task, is nondeterministic. That's fine with our assignment.)
 2. `completed_tasks`: `HashMap` of task name to its finalization time point
    (not duration). For the purpose of the main algorithm as described below, it
    would suffice to have a `HashSet` (or a `Vec`) of task name instead.
    However, storing the finalization time points (of all tasks, in addition to
    `critical_path` below), helps when debugging and testing. And we would need
    those finalization times if we implement collecting the critical path during
    the second run (as per "Next development" below).
 3. `parallelism`: `usize`, maximum paralellism so far
 4. `tasks_not_started_yet`: either `HashSet<String>` of task names not started
    yet, or `HashSet<&Task>` of references to the tasks (not started yet). Using
    a reference would involve a lifetime on `TaskIterations`, but it saves us a
    visit to `TasksMap`.
 5. `critical_path`: `Option<Path`> to keep the most recent critical path
    candidate.
 6. `running_time`: the execution from the beginning of the very first task(s).

## Algorithm: Parsing
I don't see a need to use `nom` or similar crate given the scope. My first
option is standard `&str`, its `.char_indices()` and `Peekable` iterator.

Load the file into a `String`. Split by lines. Use `.enumerate()` to keep track
of line numbers. Iterate, possibly with `.fold()` that simulates a state
machine. The state would be either `ParserState` on its own, or a tuple of
`ParserState` and possibly a few housekeeping items. (If the tuple gets too
wide, make it a `struct` on its own.)

On each line, use `line_str_slice.char_indices().peek()` to keep track of
columns and to peek one character ahead (which should be sufficient enough).

Mostly flexible/optimistic. Use question mark operator to short-circuit if
possible. Implement by two loops based on `Iterator::fold(...)`. One state (for
the outer loop) advances across lines, and its part state (for the inner loop)
advances across (sequences of) characters within a line and (in case of
`"after"` keyword and its list of prerequisite task names) across lines. Types
for those state machines will be documented in-source by identifier names and
comments. Consequences of two loops rather than one:
 - Clearer/more natural handling line by line (if `"after"` keyword, or any of
   its prerequisite task names, are on the next line(s)), but
 - Less flexible with line breaks (we require the oval parenthesis and duration
   to be on the same line as the name of the task being defined).

As implied by task names and `"after"` keyword in the example input, task names and `"after"` keyword are treated as case-sensitive.

To keep this simple, we require the left and right oval parenthesis (and the
duration between them) to be on the same line as the task name of the task being
defined. (The square left bracket (just after "after" keyword) may be on a new
line, as indicated in the example. We allow the same for square right bracket
after the last prerequisite task name: it may be on a new line.)

We accept an empty list of prerequisites: that is, right of `"after"` keyword we
accept a par of square brackets `[` and `]` with nothing (other than whitespace,
including line breaks) between them.

The parser is Unicode/non-ASCII friendly, except that
 - the error locations (columns) are reported based on ASCII/byte, and
 - the leading whitespace before "after", "[" or any prerequisites continuing on successive lines have to be ASCII whitespaces only.

## Algorithm: Finding the critical path etc.
(Very sudo-code, this does not necessarily match the types, fields or functions
exactly, and may need to be refined.)
 1. Parse the input, create `Task` instances, store in a (single instance of)
    `TasksMap`, report an error if any.
 2. Optional optimization: Collect direct dependents into a (single instance of)
    `DependentsMap`. If we skip this step, then wherever we mention `direct_dependents`
    (of a given task) below, we need to iterate over all (leftover) tasks and
    collect direct dependents (of that given task).
 3. Loop until all tasks are in `completed_tasks`. For any active (incomplete)
 path, keep the whole chain of its prerequisites (`after` chain) and the
 finalization time point of its task currently being processed. Keeping the
 whole chain does mean a lot of `clone()` as paths split (when a `Task` has
 multiple dependents). However, it allows easier debugging and reporting, and
 the overall result (the critical path(s)) will be known at the end of this
 (one) loop with its state in `TasksIteration`.
    1. Create a `TasksIteration` instance for this loop. Start with zero
     `parallelism`, all tasks in `tasks_not_started_yet` and zero
      `running_time`.
    2. Activate any tasks from `tasks_not_started_yet` that have no dependents:
 Create one "active" `Path` instance for each, with that task name as the only
 task on that path, and with that task's duration as its completion time. Put
  them in `active_paths`. Store the shortest duration of those activated tasks
 as `step_size`.
    3. Increment `running_time` by `step_size.`
    4. Iterate over all active paths, compare their finalization time point to
 `running_time`. Note any tasks that just became completed (where those two time
 values equal).
    5. Take any just completed tasks (from `active_paths`), check any `direct_dependents`.
 See if there are any `direct_dependents`, and if all their other prerequisites `after`
 were completed already - so this task was their only leftover prerequisite -
 those are to become active. (Note: This could be optimized more by removing
 leftover prerequisites as we go, but that's for far future.)
       1. Store the just completed task's name and its finalization time point
          (current `running_time`) in `completed_tasks`.
       2. Take the just completed task's `Path` out of `active_paths`, and store
          it in `critical_path`. The last one stored will have the latest
          finalization time point, hence it will be the critical one.
       3. For any direct dependents of the just completed task that have all their dependencies ("after") completed, clone that previous
         `Path` (that is now in `critical_path`). Append to it that dependent
          (new) task name. Set its finalization time to `running_time + the new
          task's duration`.
       4. Add that new `Path` to `active_paths`.
    6. Update `paralellism` to the maximum of existing `parallelism` and the
 size of `active_paths`.
    7. Update `step_size` to be the shortest leftover duration of any of the
 `active_paths`.
    8. Loop back (to #3.iii) until `active_paths` becomes empty.
    9. Your results consists of the final `parallelism`, `critical_path` and
       `running_time`.

This handles multiple root tasks naturally.

It's non-deterministic about which one of multiple critical paths it chooses.
(That could be prevented by using https://github.com/contain-rs/linked-hash-map,
which, even only at version 0.5.6, seems maintained and with a community.)

The solution will not optimize/balance/spread out `parallelism`. For some inputs
(that have short parallel chains, as compared to the critical path(s)), we could
achieve same total duration with various levels of `paralellism`. This will not
return the minimum `parallelism` required to deliver the same total duration,
but it will return the maximum `parallelism` possible.

# Next development
The biggest win would be not by moving from `String` to string slice. Neither by
implementing one of the two relationships between tasks by references (rather
than task names). Why? Because that's all `O(N)`.

Instead, we look at `Path` chains, and saving `String.clone()` - both a memory
and speed saving, potentially up to `O(N^2)` (if the task dependency tree is
close to a balanced tree).

Consider not keeping the whole chains of task names in `Path` instances, but
only the most recent task name. The downside is that to get the critical paths,
we'd need a second loop to collect all prerequisites (`after`) of the last
completed task. But since we have completion time of all tasks recorded in
`completed_tasks`, at every step (of collecting the critical path) we choose
only one of the previous tasks. Collecting the critical path (one of them if
multiple) would be `O(N)`.

Only then consider not using `String` instances, but string slices (`&str`)
instead.

I didn't do a thorough calculation, but my gut feeling is that even with all
that the overall time complexity would be `O(N^2)`.

# Testing
In addition to testing the whole pipeline with input & output files, add
`#[cfg(test)]` modules as needed.
