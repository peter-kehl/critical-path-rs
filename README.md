A standalone shell application. It reads a file, and parses it as a list of task names, durations and dependencies (other tasks from the same file). If there is an error in the input, this reports the line and column. If the input is valid, this calculates the total duration, it prints the critical path (any one if there are multiple), and the maximum number of tasks that could be run in parallel.

The tasks in the input are guaranteed not to be cyclic.

Invoke it with the input file's path. On Unix systems you can use the `./schedule-tasks` symlink, for example `./schedule-tasks test/basic.in`.

See [src/test/](src/test/) and [test/](test/) for more.

Would you like to use it with standard input instead (for example, to pipe an output of another command to it)? Then (on Unix systems) invoke it as `./schedule-tasks /dev/stdin`.
