# Debugging

This page discusses how to debug a Shake-based build system -- how to determine why it is going wrong. The page is presented as a series of common problems and how they can be investigated. The common techniques for solving any Shake problem are:

* Switch to single-threaded operation, using `-j1`. While concurrency is rarely the cause of the bugs, it often makes the diagnostic messages easier to interpret, and makes reproduction more deterministic.
* Turn on `--lint`, which will check for a number of common mistakes.
* Turn up the verbosity level `-v` will print all command lines, `-vv` will print even more, and `-vvv` gives a super-detailed commentary on everything that happens (usually for debugging Shake itself).
* If you use `shakeArgs` then every rule can be built individually, e.g. `runhaskell MyShakeFile.hs foo/output_that_fails.txt`. Reducing the error can allow higher levels of verbosity.
* If you are still stuck, ask on [StackOverflow](https://stackoverflow.com/questions/tagged/shake-build-system) using the tag `shake-build-system`.

## Raises an error

Most Shake errors are formatted as:

```
Error when running Shake build system:
* stack_element1.txt
* stack_eLement2.txt
* real_file.o
Error, rule finished running but did not produce file:
  real_rile.o
```

The lines starting with `*` provide a stack trace, where `stack_element1.txt` depended on `stack_element2.txt`, which in turn depended on and ran the rule for `real_file.o`. The specific error in this example is that the rule for producing `real_file.o` failed to actually create the file - either the file was not created, or was creating in the wrong place.

All Shake errors contain the dependency stack, which is usually the most important thing about diagnosing an error. Often the first step is to map the bottom entry on the stack to the rule that executes to produce it - e.g. `*.o` or similar.

Some typical errors you may encounter are:

* **recursion detected** - when a build system, through a series of dependencies, ends up depending on itself -- usually the stack makes it clear where the surprise is. Occasionally you will get an error about __indirect__ recursion, in which case rebuild with `-j1` and it will often give a better stack trace.
* **file does not exist and no rule available** - occurs when you have done `need` on a file such as `file.o`, but there is no pattern that matches (e.g. no `*.o` rule), and the file is not a source file (which do not need explicit patterns). Usual solution is to add a pattern that builds the artefact.
* **key matches multiple rules** - occurs when you multiple patterns that match a single file, which is not allowed - try making your rules more specific so they are disjoint.
* **system command failed** - occurs when you run a system command using `cmd`, not capturing the `ExitCode`, and it either fails to start or returns a non-zero exit code. The next line should contain the system command and arguments.

## Command line does the wrong thing

Since build systems typically spend most of their time calling out to command line tools, much of the debugging effort is around getting these tools to behave properly. The easiest way to debug a system command is to avoid it entirely - tools like `rm`, `cat` and `sed` can usually be replaced with Haskell code, which can fix issues and improve portability.

The first step to debugging a command line tool is to reproduce the error outside of Shake, typically by passing `-v` to see the command line and pasting it into a shell. Unfortunately all the platform abstractions can sometimes get in the way, in particular pay attention to:

* Quoting, especially on Windows, where the conventions are difficult to fathom.
* What the command is wrapped in - specifically `Shell` causes the command to be run through `sh`/`cmd`, anything such as `stack exec` can also add translation layers.
* Environment issues such as current directory and environment variables.
* Any output flags, such as `-o`, to control where the output goes.
* Precise details - things like using a short flag vs long flag, ordering of flags etc. can all have an impact.

Alas, each tool is different, and all present unique challenges. Where possible copy the command lines from other systems, rather than trying to rediscover this arcane knowledge.

## Weird things are rebuilding

See the [HTML Profile Reports](Profiling.md#html-profile), which provide overviews of dependencies. These can be mined to figure out the causes of rebuilding.
