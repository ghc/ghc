# Command line flags

Shake features a wide variety of command line flags, all shown with `--help`. Most of these flags map down to the `ShakeOptions` data type, and will be interpreted as such by `shakeArgsWith`.

## Modes

There are a number of different modes in which Shake can run, which have an overarching effect on what Shake does. None of these are directly available from the Shake API.

* `-h, --help` -- print the a message about supported command line flags and exit.
* `-v, --version, --numeric-version` -- print version information and exit. This will always be the Shake version number as available on Hackage.
* `--demo` -- run in demo mode, which creates a demo project and compiles it, producing [this output](Demo.md).
* `-f FILE, --file=FILE, --makefile=FILE` -- read in `FILE` as a Makefile or [Ninja project](Ninja.md). By default will look for `build.ninja` and use that if it exists.
* `-t TOOL, --tool=TOOL` -- Ninja-compatible tools such as `compdb`, only valid when operating on a Ninja project.

## What gets built

By default all `want`/`action` statements in `Rules` will be run. If any non-flag arguments are passed then these statements will be ignored, and the non-flag arguments will be passed to `want`. Calling `want` will build either `phony` rules or file rules (through `%>`). The remaining flags in this section control when things get built.

* `--no-build` -- don't build anything, useful for profiling the previous run.
* `-k, --keep-going` -- keep going if there are errors, failing only once nothing further can be built, sets `shakeStaunch`. Also known as staunch mode. Use `-S, --no-keep-going, --stop` to turn off.
* `--skip-commands` -- try and avoid running external programs, sets `shakeRunCommands` to `False`. Useful for [profiling](Profiling.md).
* `--rule-version=VERSION` -- set the `shakeVersion`, if this field changes everything will rebuild -- useful for the version of the build system, rather than the source files.
* `--no-rule-version` -- ignore the `shakeVersion` setting, equivalent to setting `shakeVersionIgnore`, useful if you compute `shakeVersion` as the hash of the build system and want to ignore changes temporarily.
* `--digest, --digest-and, --digest-and-input, --digest-or, --digest-not` -- set the `shakeChange` field to one of the `Change` values. Defaults to comparing files by modification time, but `--digest-and` (compare all files using a digest) or `--digest-and-input` (compare all input files using a digest) are often useful. However, this value is usually set in the code.

In addition, there are a number of flags for controlling whether files are treated as dirty despite being clean, or clean despite being dirty. The exact semantics remain somewhat confusing, and should be cleaned up.

## What gets printed

These options control what gets printed to the console. All output produced by Shake goes through the `shakeOutput` function, which provides another point to control the output.

* `--color, --colour` -- colorize the output, implemented via `shakeOutput`.
* `-a FULL=SHORT, --abbrev=FULL=SHORT` -- use abbreviation in status messages. Adds to the `shakeAbbreviations` field. A list of substrings that should be abbreviated in status messages, and their corresponding abbreviation. Commonly used to replace the long paths (e.g. `.make/i586-linux-gcc/output`) with an abbreviation (e.g. `$OUT`).
* `-d[ FILE], --debug[=FILE]` -- print lots of debugging information, optionally to a file.
* `-q, --quiet` -- don't print much, decreases the `shakeVerbosity` level.
* `-s, --silent` -- don't print anything, sets the `shakeVerbosity` level to `Silent`.
* `-V, --verbose, --trace` -- print more information, increases the `shakeVerbosity` level each time the flag appears, to `-VVV` gives `Diagnostic` verbosity level.
* `--no-time` -- don't print the build time after execution.

## Reporting

Shake can generate a number of different reports to help examine additional aspects of a build system.

* `-r[ FILE], --report[=FILE], --profile[=FILE]` -- generate a profile report to `FILE`, defaults to `report.html`, sets `shakeReport`. The report extension determines the type of data that is written, for example `.html` for an HTML report, or `.json` for the raw JSON data. Pass `-` as `FILE` to print a summary report to the console. To interpret the reports and for future settings see the [profiling page](Profiling.md). Can be disabled with `--no-reports`.
* `--live[=FILE]` -- list the files that are still "live" to `FILE`, defaults to `live.txt`, sets `shakeLiveFiles`. After the build system completes, write a list of all files which were live in that run, i.e. those which Shake checked were valid or rebuilt. Produces best answers if nothing rebuilds.
* `-p[N], --progress[=N]` -- show progress messages every `N` seconds, defaulting to 5 seconds. Sets `shakeProgress` to `progressSimple`. Progress messages will be shown in the window titlebar. Can be disabled by passing `--no-progress`.
* `--storage` -- write a storage log to the next to a file location controlled by `shakeFiles`/`--metadata`. This storage log will be written to occasionally -- whenever a partially truncated database is found, or when the database is compacted. Generally useful only for debugging.

## Linting

Shake can perform additional checks on an execution using linting.

* `-l, --lint` -- perform limited lint checking after the run, sets `shakeLint` to `LintBasic`. Checks that the current directory does not change and that results do not change after they are first written. Any calls to `needed` will assert that they do not cause a rule to be rebuilt. Can be disabled with `--no-lint`.
* `--lint-fsatrace` -- use the `fsatrace` program to do validation, sets `shakeLint` to `LintFSATrace`, tracking which files are accessed by command line programs.

## General settings

Finally there are some general settings, which control miscellaneous features of Shake.

* `-j[N], --jobs[=N]` -- allow `N` jobs/threads to run at, sets `shakeThreads`. Defaults to 1 thread if the flag is omitted, or the number of CPUs if passing `-j` with no argument value.
* `-m PREFIX, --metadata=PREFIX` -- prefix for storing Shake produced metadata files, sets `shakeFiles`. Generally this setting should be controlled in the code.
* `--flush=N` -- sets `shakeFlush`, defaults to `--flush=10`. How often to flush Shake metadata files in seconds. It is possible that on abnormal termination (not Haskell exceptions but killing processes) that any rules that completed in the last `N` seconds will be lost. Flushing can be disabled entirely with `--never-flush`, if you like living on the edge.
* `-C DIRECTORY`, `--directory=DIRECTORY` -- change to `DIRECTORY` before doing anything.
* `-w, --print-directory` -- print the current directory after changing directory, can be disabled with `--no-print-directory` (these flags are in `make` -- probably for historical reasons).
* `--sleep` -- sleep for one second before building.
* `--exception` -- if set throws exceptions, otherwise exceptions will be printed to `stderr` and it will throw `ExitFailure`.
