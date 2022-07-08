//! All indices here are ASCII-based, as per
//! https://doc.rust-lang.org/std/primitive.str.html#method.char_indices.
use crate::{Dependencies, Task, TasksMapNotValidated, TasksMapValidated};
use core::iter::Peekable;
use core::mem::swap;
use std::fmt::{self, Display, Formatter};
use std::fs;
use std::num::IntErrorKind;
use std::str::CharIndices;

const AFTER: &str = "after";
/// The minimum number of ASCII whitespace required to indent "after", [ or any
/// prerequisite task names on successive lines.
const INDENT_WHITESPACE_COUNT: usize = 2;

pub type ParserResult<T> = Result<T, ParserError>;

// We could use NonZeroUsize, but no real need for it.
/// For errors that point to the input (hence used only while parsing input).
/// See also `ParserError::new()`.
#[derive(Debug, PartialEq)]
pub struct ParserError {
    line: usize,
    /// Column index, based on ASCII/bytes (but not necessarily Unicode).
    column: usize,
}

impl ParserError {
    /// Create a new Error instance. `line` and `column` are the exact as to be
    /// reported (hence, starting from 1).
    pub fn new(line: usize, column: usize) -> Self {
        debug_assert!(line > 0);
        debug_assert!(column > 0);
        Self { line, column }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Error: line {}, column {}", self.line, self.column)
    }
}

/// An error on the line currently being parsed. If the field is `Some`, it
/// carries the column position (starting from 1). If the field is `None`, the
/// error was either at the end of the line, or it's up to the caller to
/// determine based on their use case.
#[derive(Debug)]
struct LineError(Option<usize>);

impl LineError {
    pub fn new() -> Self {
        Self(None)
    }

    /// Param `column` starts from 1.
    pub fn new_at(column: usize) -> Self {
        Self(Some(column))
    }
}

type LineResult<T> = Result<T, LineError>;

#[derive(Debug)]
#[allow(clippy::enum_variant_names)]
enum LineErrorOrEnd<A> {
    LineErr(LineError),
    LineEndNotAcceptable,
    /// Not representing an error, but short-circuiting the question mark
    /// operator even on success. That is fine as per
    /// https://doc.rust-lang.org/nightly/core/ops/trait.Try.html. @TODO Once
    /// core::ops::Try becomes stable, use it instead.
    LineEndAcceptable(A),
}

// On nightly channel we could implement core::ops::Try and core::ops::Residual.
type LineResultOrLineEnd<T> = Result<T, LineErrorOrEnd<ParserState>>;

impl<A> From<LineError> for LineErrorOrEnd<A> {
    fn from(line_error: LineError) -> Self {
        Self::LineErr(line_error)
    }
}

/// Stage when parsing input for one `Task`. Formerly called `ParserState`. Each
/// variant indicates the next expected part.
#[derive(PartialEq, Debug, Clone, Copy)]
enum TaskParserStage {
    /// Name of the task to be defined. (Not a prerequisite task right of
    /// "after" keyword.)
    TaskDef,
    /// "after" keyword. The bool field indicates whether this "after" keyword
    /// must be indented by whitespace as per `INDENT_WHITESPACE_COUNT` (if it
    /// were proceeded by line break).
    AfterKeyword(bool),
    OpenSquareBracket,
    /// Expecting a prerequisite (one of task name(s) between `[` and `]`,
    /// (excluding the square brackets). Again, the bool field indicates whether
    /// this (prerequisite) task name must be indented by whitespace as per
    /// `INDENT_WHITESPACE_COUNT` (if it were proceeded by line break).
    Prerequisite(bool),
    // Processing after all the input was consumed. This stores the very last
    // task that has been collected, but not stored yet (if any). The bool field
    // here indicates whether this stage was completed.
    EndOfInput(bool),
}

/// Task information parsed so far.
#[derive(PartialEq, Debug, Clone)]
struct ParsedTaskInfo {
    name: Option<String>,
    duration: Option<usize>,
    /// Keys are dependency names (as listed right of `after`). Values are line
    /// & column numbers (starting from 0) of where that dependency was defined.
    /// That's required for error reporting later.
    after: Dependencies<(usize, usize)>,
}

impl ParsedTaskInfo {
    pub fn new() -> Self {
        Self {
            name: None,
            duration: None,
            after: Dependencies::new(),
        }
    }
}

/// Peek/advance `char_indices`, until the nearest peeked character, or end of
/// line, satisfies the given `condition` (for which `condition(...)` returns
/// `true`). Parameter `condition(...)` is called with `Some(char)` of any
/// character, until `condition(...)` returns `true` - and then this function
/// returns `(true, Some(char_index))`, or until end of line. If we reach end of
/// line (and `condition(...)` hasn't returned `true` yet), this calls
/// `condition(None)`, and if that returns `true`, this function returns `(true,
/// None)`. Otherwise this function returns `(false, Some(last_peeked_index))`
/// if this function call peeked at least once; `(false, None)` otherwise. See
/// also `Match`.
pub fn peek(
    condition: impl Fn(Option<char>) -> bool,
    char_indices: &mut Peekable<impl Iterator<Item = (usize, char)>>,
) -> Match {
    let mut last_peeked: Option<usize> = None;
    loop {
        if let Some(&(index, c)) = char_indices.peek() {
            if condition(Some(c)) {
                return (true, Some(index));
            }
            last_peeked = Some(index);
        } else {
            return if condition(None) {
                (true, None)
            } else {
                (false, last_peeked)
            };
        }
        char_indices.next();
    }
}

/// A result indicating if there is any match (`bool`), and if so, `Some` of the
/// index it was matched at (if matched by a character), or `None` if matched
/// but with no position (for our purposes that can be only end of line).
type Match = (bool, Option<usize>);

/// Peek/advance `char_indices`, until the nearest `char_indices.peek()` is a
/// non-whitespace. Return `(true, Some(non-whitespace character index))` if
/// there is such a non-whitespace. Otherwise see `peek(...)`.
pub fn peek_non_whitespace(
    char_indices: &mut Peekable<impl Iterator<Item = (usize, char)>>,
) -> Match {
    peek(
        |char_or_none| matches!(char_or_none, Some(c) if !c.is_whitespace()),
        char_indices,
    )
}

/// Peek/advance `char_indices`, until the nearest `char_indices.peek()` is a
/// character that can't be a part of a task name (other than alphabetical,
/// underscore, hyphen or a digit). Return `(true, Some(character index))` if
/// there is such a character.  Otherwise see `peek(...)`.
pub fn peek_non_task_name(
    char_indices: &mut Peekable<impl Iterator<Item = (usize, char)>>,
) -> Match {
    peek(
        |char_or_none| {
            if let Some(c) = char_or_none {
                !c.is_alphanumeric() && !matches!(c, '_' | '-')
            } else {
                false
            }
        },
        char_indices,
    )
}

/// Peek/advance `char_indices`, until the nearest non-whitespace. Return `Ok`
/// of `Some` of the first non-whitespace character; `Err<Error>` if there is no
/// non-whitespace (before the end of string). If `accept_whitespace_line` is
/// true and there is no non-whitespace, this calls `state_on_line_end()` and
/// returns its value inside `Err(LineErrorOrEnd::LineErr(...))`, which can
/// serve to short-circuit the caller.
fn skip_whitespace_impl(
    char_indices: &mut Peekable<CharIndices>,
    accept_whitespace_line: bool,
    state_on_line_end: impl Fn() -> ParserState,
) -> LineResultOrLineEnd<Option<char>> {
    let (matched, _char_or_none) = peek_non_whitespace(char_indices);
    if matched {
        Ok(Some(
            char_indices
                .peek()
                .expect("non-whitespace character with index")
                .1,
        ))
    } else if accept_whitespace_line {
        Err(LineErrorOrEnd::LineEndAcceptable(state_on_line_end()))
    } else {
        Err(LineErrorOrEnd::LineEndNotAcceptable)
    }
}

/// Skip whitespace, return `Ok` with the first non-whitespace character. If
/// there is none (when it reaches line end), return `Err`.
fn skip_whitespace(char_indices: &mut Peekable<CharIndices>) -> LineResultOrLineEnd<Option<char>> {
    skip_whitespace_impl(char_indices, false, || unimplemented!())
}

/// Skip whitespace, return `Ok` with the first non-whitespace character. If
/// there is none (when it reaches line end), return a clone of `new_state`
/// inside `Err(LineErrorOrEnd::LineErr(...))`. That serves for short-circuiting
/// with question mark operator.
fn skip_whitespace_or_line_end(
    char_indices: &mut Peekable<CharIndices>,
    new_state: &ParserState,
) -> LineResultOrLineEnd<Option<char>> {
    skip_whitespace_impl(char_indices, true, || ParserState {
        line_processed: true,
        ..new_state.clone()
    })
}

/// Skip whitespace, return `Ok` with the first non-whitespace character. If
/// there is none (when it reaches line end), return a clone of
/// `new_state_on_line_end` with its `task_stage` set to `new_stage_on_line_end`
/// and `line_processed` to `true`, wrapped inside
/// `Err(LineErrorOrEnd::LineErr(...))`. That serves for short-circuiting with
/// question mark operator.
fn skip_whitespace_or_line_end_stage(
    char_indices: &mut Peekable<CharIndices>,
    new_state_on_line_end: &ParserState,
    new_stage_on_line_end: TaskParserStage,
) -> LineResultOrLineEnd<Option<char>> {
    skip_whitespace_impl(char_indices, true, || ParserState {
        task_stage: new_stage_on_line_end,
        line_processed: true,
        ..new_state_on_line_end.clone()
    })
}

/// Represents state of a state machine parsing tokens, possibly across multiple
/// lines. Suitable to use with `Iterator::fold()`.
#[derive(Clone, Debug)]
struct ParserState {
    line_processed: bool,
    task_stage: TaskParserStage,
    info: ParsedTaskInfo,
    tasks: TasksMapNotValidated,
}

impl ParserState {
    pub fn new() -> Self {
        Self {
            line_processed: false,
            task_stage: TaskParserStage::TaskDef,
            info: ParsedTaskInfo::new(),
            tasks: TasksMapNotValidated::new(),
        }
    }
}

/// Convert the result. If `line_result_or_line_end` matches
/// `Err(LineErrorOrEnd::LineEndAcceptable(_)), then convert it to `Ok` of the
/// wrapped state - for short-circuiting upstream.
fn line_part_result_to_parser_result(
    line_result_or_line_end: LineResultOrLineEnd<ParserState>,
    line_num: usize,
    line_len: usize,
    char_indices: &mut Peekable<impl Iterator<Item = (usize, char)>>,
) -> ParserResult<ParserState> {
    match line_result_or_line_end {
        Ok(result) => Ok(result),
        Err(LineErrorOrEnd::LineErr(LineError(Some(column)))) => {
            Err(ParserError::new(line_num, column))?
        }
        Err(LineErrorOrEnd::LineErr(LineError(None)))
        | Err(LineErrorOrEnd::LineEndNotAcceptable) => {
            let col = match char_indices.peek() {
                Some(&(col, _)) => col,
                None => line_len,
            };
            Err(ParserError::new(line_num, col))?
        }
        Err(LineErrorOrEnd::LineEndAcceptable(new_state)) => Ok(new_state),
    }
}

/// Parse the whole input. Return an `Ok` with a `TasksMap` on success; `Err`
/// with an `Error` otherwise. On top of DESIGN.md:
/// - max. one task (being defined) per line
/// - if a task's prerequisites start/continue on other line(s), those lines
///   must be indented by whitespace (as per `INDENT_WHITESPACE_COUNT`)
/// - on empty/whitespace-only input, return success (which will lead to an
///   empty critical path)
pub fn parse_input(input_text: &str) -> ParserResult<TasksMapNotValidated> {
    // We could use input_text.lines().enumerate() to get line numbers, but we
    // need the last line number outside the loop, too. Hence we advance line
    // number ourselves. Increased on each iteration over lines, so when read
    // from, it starts counting from 1.
    let mut line_num: usize = 0;
    // Updated (to be the length of the current line) on each iteration over
    // lines, so when read from, it reflects length of the current or last line.
    let mut line_len: usize = 0;

    let final_state = input_text
        .lines()
        .try_fold(ParserState::new(), |state, line| {
            line_num += 1;
            line_len = line.len();

            parse_input_iteration(state, line, line_num)
        })?;
    if !matches!(
        final_state.task_stage,
        TaskParserStage::TaskDef | TaskParserStage::AfterKeyword(_)
    ) {
        Err(ParserError::new(line_num, line_len))
    } else {
        parse_input_handle_final_stage(final_state, line_num, line_len)
    }
}

fn parse_input_iteration(
    mut state: ParserState,
    line: &str,
    line_num: usize,
) -> ParserResult<ParserState> {
    let line_len = line.len();

    let mut char_indices = line.char_indices().peekable();

    state.line_processed = false;
    while !state.line_processed {
        let line_result_or_line_end =
            parse_line_part(state.clone(), line, line_num, &mut char_indices);
        let state_or_error = line_part_result_to_parser_result(
            line_result_or_line_end,
            line_num,
            line_len,
            &mut char_indices,
        );
        state = state_or_error?;
    }
    Ok(state)
}

fn parse_input_handle_final_stage(
    final_state: ParserState,
    number_of_lines: usize,
    last_line_len: usize,
) -> ParserResult<TasksMapNotValidated> {
    let end_line_part_result = parse_line_part(
        ParserState {
            task_stage: TaskParserStage::EndOfInput(false),
            ..final_state
        },
        "",
        number_of_lines,
        &mut "".char_indices().peekable(),
    );

    let end_parser_result = line_part_result_to_parser_result(
        end_line_part_result,
        number_of_lines,
        last_line_len,
        &mut "".char_indices().peekable(),
    );
    match end_parser_result {
        Ok(state) => {
            debug_assert!(matches!(
                state.task_stage,
                TaskParserStage::EndOfInput(true)
            ));
            Ok(state.tasks)
        }
        Err(err) => Err(err),
    }
}

/// Peek/advance `char_indices` until the nearest task name or "after". Return
/// `Ok` with that substring's start and (exclusive) end, or `Err<Error>`
/// otherwise.
fn peek_task_or_after_or_number_idxs(
    char_indices: &mut Peekable<CharIndices>,
) -> LineResult<(usize, usize)> {
    if let (true, Some(task_or_after_idx)) = peek_non_whitespace(char_indices) {
        let non_task = peek_non_task_name(char_indices);
        if let (found, Some(non_task_name_idx)) = non_task {
            let non_task_name_idx = if found {
                non_task_name_idx
            } else {
                non_task_name_idx + 1 // not found = reached end of string, peek returned that last index, but here we want one step more.
            };
            if task_or_after_idx < non_task_name_idx {
                Ok((task_or_after_idx, non_task_name_idx))
            } else {
                debug_assert_eq!(task_or_after_idx, non_task_name_idx);
                // The first non-whitespace character can't be a part of a task
                // name
                Err(LineError::new_at(task_or_after_idx + 1))
            }
        } else {
            Err(LineError::new())
        }
    } else {
        debug_assert!(char_indices.peek().is_none()); // end of line
        Err(LineError::new())
    }
}

/// Expect the given character, after any whitespace or new line(s). Consume. On
/// new line, return a short-circuiting error indicating line end. Then it's
/// responsibility of the caller to loop back and invoke this function again. If
/// the first non-whitespace character is other than `expected`, return an error
/// with that offending character's column.
fn expect_and_consume(
    state: &ParserState,
    char_indices: &mut Peekable<CharIndices>,
    expected: char,
) -> LineResultOrLineEnd<()> {
    // Allow multiple line breaks before
    skip_whitespace_or_line_end(char_indices, state)?;
    {
        let non_whitespace_match = peek_non_whitespace(char_indices);
        // If there was only whitespace, this would have been skipped.
        debug_assert!(non_whitespace_match.0);
        debug_assert!(non_whitespace_match.1.is_some());
        if !matches!(char_indices.peek(), Some((_, actual)) if *actual==expected) {
            Err(LineError::new_at(non_whitespace_match.1.unwrap() + 1))?
        }
    }
    char_indices.next();
    Ok(())
}

/// Don't call it in between handling a task name (of the task being defined),
/// and its duration. If stored, this  clears/resets `state.info`.
fn store_task_info_if_possible(state: &mut ParserState) {
    // We should have collected either both state.info.name and
    // state.info.duration, or none of them.
    debug_assert_eq!(state.info.name.is_some(), state.info.duration.is_some());
    // Check state.info.name and state.info.duration. If both .is_some() - from
    // the previous run - then create Task, store it in a map, reset state.info.
    if state.info.name.is_some() {
        let mut name = None;
        swap(&mut state.info.name, &mut name);
        let name = name.unwrap();

        let mut after = Dependencies::new();
        swap(&mut state.info.after, &mut after);

        let duration = state.info.duration.unwrap();
        state.info.duration = None;
        state
            .tasks
            .insert(name.clone(), Task::new(name, duration, after));
    }
    state.info.name = None;
}

/// Parse a part (or the whole rest) of `line`. Collect parts of `info` or
/// construct a `Task` and store it in `result`, and transition to a new state,
/// as appropriate. Return `Some` of the new state, or `Err` with an error.
/// (Used with `Iterator.fold`).
fn parse_line_part(
    mut state: ParserState,
    line: &str,
    line_num: usize,
    char_indices: &mut Peekable<CharIndices>,
) -> LineResultOrLineEnd<ParserState> {
    use TaskParserStage::*;
    // Unless indicated otherwise, `usize` variables, or variables with names
    // ending with `_idx` or `_start` or `_end`, here indicate start (byte)
    // index of a string/pattern being parsed/checked (starting from 0). Ones
    // with `_end` postfix are one character pass
    match state.task_stage {
        TaskDef => parse_line_part_task_def(state, line, char_indices),
        AfterKeyword(require_spaces) => {
            parse_line_part_after_keyword(state, line, char_indices, require_spaces)
        }
        OpenSquareBracket => {
            expect_and_consume(&state, char_indices, '[')?;
            state.task_stage = TaskParserStage::Prerequisite(false);
            Ok(state)
        }
        Prerequisite(require_spaces) => {
            parse_line_part_prerequisite(state, line, line_num, char_indices, require_spaces)
        }
        EndOfInput(completed) => {
            debug_assert!(!completed);
            // Process any pending state.info, if state.info.name &
            // state.info.duration are set.
            store_task_info_if_possible(&mut state);
            state.task_stage = EndOfInput(true);
            Ok(state)
        }
    }
}

fn parse_line_part_task_def(
    mut state: ParserState,
    line: &str,
    char_indices: &mut Peekable<CharIndices>,
) -> LineResultOrLineEnd<ParserState> {
    // Allow multiple line breaks between task definitions, or between a task
    // definition and its "after" keyword.
    skip_whitespace_or_line_end(char_indices, &state)?;
    {
        let (task_idx, non_task_name_idx) = peek_task_or_after_or_number_idxs(char_indices)?;
        let task = &line[task_idx..non_task_name_idx];

        if task == AFTER {
            Err(LineError::new_at(task_idx + 1))?
        }
        // On top of DESIGN.md: First character of a task name has to be
        // alphabetic, only the rest may be digits, underscore or hyphen.
        if !matches!(task.chars().next(), Some(c) if c.is_alphabetic()) {
            Err(LineError::new_at(task_idx + 1))?
        }

        // If we did collect a task (any its "after" prerequisites, if any) on
        // the previous line(s), we can (in general) finish handling it only now
        // (we could have finish it earlier if the task had "after" and it was
        // already closed with `]` bracket, but let's handle this generally
        // here). So now we store such a collected task.
        store_task_info_if_possible(&mut state);
        state.info.name = Some(task.to_owned());
    }

    if skip_whitespace(char_indices)? != Some('(') {
        Err(LineError::new())?
    }
    char_indices.next();
    {
        let (duration_start, duration_end) = peek_task_or_after_or_number_idxs(char_indices)?;
        let duration = &line[duration_start..duration_end];
        let parsed_duration = duration.parse::<usize>();
        if let Err(duration_error) = parsed_duration {
            // Index of the offending char is unknown, so let's find it.
            // Multiple consecutive zero's ("00", "000"...) are parsed OK. Empty
            //string (no digits at all) is handled already above. Other than
            //(positive) overflow, all we do is check for non-digit characters
            //(including minus sign, too).
            let error_idx = if duration_error.kind() == &IntErrorKind::PosOverflow {
                duration_start + 1
            } else if let Some((offending_idx, _)) =
                duration.char_indices().find(|(_idx, c)| !c.is_numeric())
            {
                duration_start + offending_idx + 1
            } else {
                line.len()
            };
            return LineResultOrLineEnd::Err(LineErrorOrEnd::LineErr(LineError::new_at(error_idx)));
        }

        debug_assert!(state.info.duration.is_none());
        state.info.duration = Some(parsed_duration.unwrap());
    }
    if skip_whitespace(char_indices)? != Some(')') {
        Err(LineError::new())?
    }
    char_indices.next();
    state.task_stage = TaskParserStage::AfterKeyword(false);
    Ok(state)
}

fn parse_line_part_after_keyword(
    mut state: ParserState,
    line: &str,
    char_indices: &mut Peekable<CharIndices>,
    require_spaces: bool,
) -> LineResultOrLineEnd<ParserState> {
    // Allow multiple line breaks before a prerequisite task name, but then that
    // "after" keyword must be indented with spaces
    skip_whitespace_or_line_end_stage(char_indices, &state, TaskParserStage::AfterKeyword(true))?;

    if let Some(&(idx, _)) = char_indices.peek() {
        // TODO once https://github.com/rust-lang/rust/issues/53667 is stable,
        // move the following `if` condition and && it with the outer `if`
        // condition.
        if idx == 0 {
            // If there is no (whitespace) indentation, then it's a definition
            // of a new task instead of "after" keyword.
            state.task_stage = TaskParserStage::TaskDef;
            return Ok(state);
        }
    }
    {
        let (after_start, after_end) = peek_task_or_after_or_number_idxs(char_indices)?;
        let after = &line[after_start..after_end];
        // We report any error at the beginning of an incorrect string here
        // (rather than at the index of the offending character).
        if after != AFTER {
            Err(LineError::new_at(after_start + 1))?
        }
        if require_spaces && after_start < INDENT_WHITESPACE_COUNT {
            // DESIGN.md: assume ASCII whitespace only
            Err(LineError::new_at(after_start + 1))?
        }
        debug_assert!(state.info.after.is_empty());
    }
    char_indices.next();
    debug_assert!(state.info.after.is_empty());
    state.task_stage = TaskParserStage::OpenSquareBracket;
    Ok(state)
}

fn parse_line_part_prerequisite(
    mut state: ParserState,
    line: &str,
    line_num: usize,
    char_indices: &mut Peekable<CharIndices>,
    require_spaces: bool,
) -> LineResultOrLineEnd<ParserState> {
    // Allow multiple line breaks before a prerequisite task name, but then that
    // (very next) prerequisite must be indented with spaces
    skip_whitespace_or_line_end_stage(char_indices, &state, TaskParserStage::Prerequisite(true))?;

    if matches!(char_indices.peek(), Some((_, ']'))) {
        char_indices.next();
        // Must have nothing else (other than whitespace) on the rest of the
        // line.
        if let (true, Some(non_whitespace)) = peek_non_whitespace(char_indices) {
            Err(LineError::new_at(non_whitespace + 1))?
        }
        state.task_stage = TaskParserStage::TaskDef;
        state.line_processed = true;
        return Ok(state);
    }
    {
        let (task_start, task_end) = peek_task_or_after_or_number_idxs(char_indices)?;
        if require_spaces && task_start < INDENT_WHITESPACE_COUNT {
            // assume ASCII whitespace
            Err(LineError::new_at(task_start + 1))?
        }
        let task = &line[task_start..task_end];
        if task == AFTER {
            Err(LineError::new_at(task_start + 1))?
        }
        state
            .info
            .after
            .insert(task.to_owned(), (line_num, task_start));
    }

    if let (true, Some(non_whitespace)) = peek_non_whitespace(char_indices) {
        // Get the non-whitespace character (one just peeked at).
        let peeked = char_indices.peek();
        debug_assert!(matches!(peeked, Some((actual_idx, _)) if *actual_idx==non_whitespace));
        match peeked {
            Some((_, ']')) => {
                // Short-circuit back to the top for the rest of `]` handling.
                state.task_stage = TaskParserStage::Prerequisite(false);
                return Ok(state);
            }
            Some((_, ',')) => {
                char_indices.next();
                // Extra input flexibility on top of DESIGN.md: The comma or
                // close square bracket `]`  must be on the same line (as that
                // previous prerequisite name). But if it's a comma, it doesn't
                // need any spaces right of it (if the following prerequisite is
                // still on the same line).
                state.task_stage = TaskParserStage::Prerequisite(false);
                return Ok(state);
            }
            _ => Err(LineError::new_at(non_whitespace + 1))?,
        }
    }
    // Can't end an line here - we require comma or `]` not to be the first
    // non-whitespace on a line.
    Err(LineError::new())?
}

/// Validate that dependent names are valid task names. This does NOT check for
/// circular dependencies! Return `Ok` the same task set, but without the line &
/// column tuples in `Task::after`; or `Err` with a location of the first
/// non-existent dependency name.
fn validate_dependencies(tasks_map: &TasksMapNotValidated) -> ParserResult<()> {
    tasks_map
        .iter()
        .try_for_each(|(_dependent_name, dependent)| {
            dependent.after.iter().try_for_each(
                |(dependency_name, (line, col))| -> ParserResult<()> {
                    if !tasks_map.contains_key(dependency_name) {
                        Err(ParserError::new(*line, *col))?
                    }
                    Ok(())
                },
            )
        })
}

pub fn strip_dependency_locations(tasks_map: TasksMapNotValidated) -> TasksMapValidated {
    let mut result = TasksMapValidated::with_capacity(tasks_map.len());
    for (task_name, task) in tasks_map {
        let stripped_dependencies = task
            .after
            .into_iter()
            .map(|(dep_name, _)| (dep_name, ()))
            .collect::<Dependencies<_>>();

        result.insert(
            task_name.clone(),
            <Task<()>>::new(task.name, task.duration, stripped_dependencies),
        );
    }
    result
}

pub fn parse_input_and_validate(input_text: &str) -> ParserResult<TasksMapValidated> {
    let parsed_tasks = parse_input(input_text)?;

    validate_dependencies(&parsed_tasks)?;
    Ok(strip_dependency_locations(parsed_tasks))
}

pub fn parse_and_validate_file_path(file_path: &str) -> ParserResult<TasksMapValidated> {
    let input_text = fs::read_to_string(file_path).expect("Can't read the input file.");
    parse_input_and_validate(&input_text)
}
