# Shake Manual

_See also: [Shake links](https://github.com/ndmitchell/shake#readme); [Why choose Shake](Why.md#readme); [Function documentation](https://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html)_

Shake is a Haskell library for writing build systems -- designed as a replacement for `make`. This document describes how to get started with Shake, assuming no prior Haskell knowledge. First, let's take a look at a Shake build system:

    import Development.Shake
    import Development.Shake.Command
    import Development.Shake.FilePath
    import Development.Shake.Util

    main :: IO ()
    main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
        want ["_build/run" <.> exe]

        phony "clean" $ do
            putInfo "Cleaning files in _build"
            removeFilesAfter "_build" ["//*"]

        "_build/run" <.> exe %> \out -> do
            cs <- getDirectoryFiles "" ["//*.c"]
            let os = ["_build" </> c -<.> "o" | c <- cs]
            need os
            cmd_ "gcc -o" [out] os

        "_build//*.o" %> \out -> do
            let c = dropDirectory1 $ out -<.> "c"
            let m = out -<.> "m"
            cmd_ "gcc -c" [c] "-o" [out] "-MMD -MF" [m]
            neededMakefileDependencies m

This build system builds the executable `_build/run` from all C source files in the current directory. It will rebuild if you add/remove any C files to the directory, if the C files themselves change, or if any headers used by the C files change. All generated files are placed in `_build`, and a `clean` command is provided that will wipe all the generated files. In the rest of this manual we'll explain how the above code works and how to extend it.

#### Running this example

To run the example above:

1. Install the [Haskell Stack](https://haskellstack.org/), which provides a Haskell compiler and package manager.
2. Type `stack install shake`, to build and install Shake and all its dependencies.
3. Type `stack exec -- shake --demo`, which will create a directory containing a sample project, the above Shake script (named `Shakefile.hs`), and execute it (which can be done by `runhaskell Shakefile.hs`). For more details see a [trace of `shake --demo`](Demo.md).

## Basic syntax

This section explains enough syntax to write a basic Shake build script.

#### Boilerplate

The build system above starts with the following boilerplate:

<pre>
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
&#32;
main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    <i>build rules</i>
</pre>

All the interesting build-specific code is placed under <tt><i>build rules</i></tt>. Many build systems will be able to reuse that boilerplate unmodified.

#### Defining targets

A target is a file we want the build system to produce (typically executable files). For example, if we want to produce the file `manual/examples.txt` we can write:

    want ["manual/examples.txt"]

The `want` function takes a list of strings. In Shake lists are written `[item1,item2,item2]` and strings are written `"contents of a string"`. Special characters in strings can be escaped using `\` (e.g. `"\n"` for newline) and directory separators are always written `/`, even on Windows.

Most files have the same name on all platforms, but executable files on Windows usually have the `.exe` extension, while on POSIX they have no extension. When writing cross-platform build systems (like the initial example), we can write:

    want ["_build/run" <.> exe]

The `<.>` function adds an extension to a file path, and the built-in `exe` variable evaluates to `"exe"` on Windows and `""` otherwise.

#### Defining rules

A rule describes the steps required to build a file. A rule has two components, a <tt><i>pattern</i></tt> and some <tt><i>actions</i></tt>:

<pre>
<i>pattern</i> %&gt; \out -> do
    <i>actions</i>
</pre>

The <tt><i>pattern</i></tt> is a string saying which files this rule can build. It may be a specific file (e.g.  `"manual/examples.txt" %> ...`) or may use wildcards:

* The `*` wildcard matches anything apart from a directory separator. For example `"manual/*.txt"` would define a rule for any `.txt` file in the `manual` directory, including `manual/examples.txt`, but would not match `manual/examples.zip`, `examples.txt` or `manual/docs/examples.txt`.
* The `//` wildcard matches any number of complete path components. For example `//*.txt` would define a rule for any `.txt` file, including `manual/examples.txt`. As another example, `manual//examples.txt` would match any file named `examples.txt` inside `manual`, including both `manual/examples.txt` and `manual/docs/examples.txt`.

It is an error for multiple patterns to match a file being built, so you should keep patterns minimal. Looking at the two rules in the initial example:

    "_build/run" <.> exe %> ...
    "_build//*.o" %> ...

The first matches only the `run` executable, using `<.> exe` to ensure the executable is correctly named on all platforms. The second matches any `.o` file anywhere under `_build`. As examples, `_build/main.o` and `_build/foo/bar.o` both match while `main.o` and `_build/main.txt` do not.

Lots of compilers produce `.o` files, so if you are combining two different languages, say C and Haskell, use the extension `.c.o` and `.hs.o` to avoid overlapping rules.

The <tt><i>actions</i></tt> are a list of steps to perform and are listed one per line, indented beneath the rule. Actions both express dependencies (say what this rule uses) and run commands (actually generate the file). During the action the `out` variable is bound to the file that is being produced.

#### A simple rule

Let's look at a simple example of a rule:

    "*.rot13" %> \out -> do
        let src = out -<.> "txt"
        need [src]
        cmd_ "rot13" src "-o" out

This rule can build any `.rot13` file. Imagine we are building `"file.rot13"`, it proceeds by:

* Using `let` to define a local variable `src`, using the `-<.>` extension replacement method, which removes the extension from a file and adds a new extension. When `out` is `"file.rot13"` the variable `src` will become `file.txt`.
* Using `need` to introduce a dependency on the `src` file, ensuring that if `src` changes then `out` will be rebuilt and that `src` will be up-to-date before any further commands are run.
* Using `cmd_` to run the command line `rot13 file.txt -o file.rot13`, which should read `file.txt` and write out `file.rot13` being the ROT13 encoding of the file.

Many rules follow this pattern -- calculate some local variables, `need` some dependencies, then use `cmd_` to perform some actions. We now discuss each of the three statements.

#### Local variables

Local variables can be defined as:

<pre>
let <i>variable</i> = <i>expression</i>
</pre>

Where <tt><i>variable</i></tt> is a name consisting of letters, numbers and underscores (a-z, A-Z, 0-9 and \_). All variables _must_ start with a lower-case letter.

An <tt><i>expression</i></tt> is any combination of variables and function calls, for example `out -<.> "txt"`. A list of some common functions is discussed later.

Variables are evaluated by substituting the <tt><i>expression</i></tt> everywhere the <tt><i>variable</i></tt> is used. In the simple example we could have equivalently written:

    "*.rot13" %> \out -> do
        need [out -<.> "txt"]
        cmd_ "rot13" (out -<.> "txt") "-o" out

Variables are local to the rule they are defined in, cannot be modified, and should not be defined multiple times within a single rule.

#### File dependencies

You can express a dependency on a file with:

    need ["file.src"]

To depend on multiple files you can write:

    need ["file.1","file.2"]

Or alternatively:

    need ["file.1"]
    need ["file.2"]

It is preferable to use fewer calls to `need`, if possible, as multiple files required by a `need` can be built in parallel.

#### Running external commands

The `cmd_` function allows you to call system commands, e.g. `gcc`. Taking the initial example, we see:

    cmd_ "gcc -o" [out] os

After substituting `out` (a string variable) and `os` (a list of strings variable) we might get:

    cmd_ "gcc -o" ["_make/run"] ["_build/main.o","_build/constants.o"]

The `cmd_` function takes any number of space-separated expressions. Each expression can be either a string (which is treated as a space-separated list of arguments) or a list of strings (which is treated as a direct list of arguments).  Therefore the above command line is equivalent to either of:

    cmd_ "gcc -o _make/run _build/main.o _build/constants.o"
    cmd_ ["gcc","-o","_make/run","_build/main.o","_build/constants.o"]

To properly handle unknown string variables it is recommended to enclose them in a list, e.g. `[out]`, so that even if `out` contains a space it will be treated as a single argument.

The `cmd_` function as presented here will fail if the system command returns a non-zero exit code, but see later for how to treat failing commands differently.

#### Filepath manipulation functions

Shake provides a complete library of filepath manipulation functions (see the [docs for `Development.Shake.FilePath`](https://hackage.haskell.org/package/shake/docs/Development-Shake-FilePath.html)), but the most common are:

* `str1 </> str2` -- add the path components together with a slash, e.g. `"_build" </> "main.o"` equals `"_build/main.o"`.
* `str1 <.> str2` -- add an extension, e.g. `"main" <.> "o"` equals `"main.o"`.
* `str1 ++ str2` -- append two strings together, e.g. `"hello" ++ "world"` equals `"helloworld"`.
* `str1 -<.> str2` -- replace an extension, e.g. `"main.c" -<.> "o"` equals `"main.o"`.
* `dropExtension str` -- drop the final extension of a filepath if it has one, e.g. `dropExtension "main.o"` equals `"main"`, while `dropExtension "main"` equals `"main"`.
* `takeFileName str` -- drop the path component, e.g. `takeFileName "_build/src/main.o"` equals `"main.o"`.
* `dropDirectory1 str` -- drop the first path component, e.g. `dropDirectory1 "_build/src/main.o"` equals `"src/main.o"`.

## Advanced Syntax

The following section covers more advanced operations that are necessary for moderately complex build systems, but not simple ones.

#### Directory listing dependencies

The function `getDirectoryFiles` can retrieve a list of files within a directory:

    files <- getDirectoryFiles "" ["//*.c"]

After this operation `files` will be a variable containing all the files matching the pattern `"//*.c"` (those with the extension `.c`) starting at the directory `""` (the current directory). To obtain all `.c` and `.cpp` files in the src directory we can write:

    files <- getDirectoryFiles "src" ["//*.c","//*.cpp"]

The `getDirectoryFiles` operation is tracked by the build system, so if the files in a directory change the rule will rebuild in the next run. You should only use `getDirectoryFiles` on source files, not files that are generated by the build system, otherwise the results will change while you are running the build and the build may be inconsistent.

#### List manipulations

Many functions work with lists of values. The simplest operation on lists is to join two lists together, which we do with `++`. For example, `["main.c"] ++ ["constants.c"]` equals `["main.c", "constants.c"]`.

Using a _list comprehension_ we can produce new lists, apply functions to the elements and filtering them. As an example:

    ["_build" </> x -<.> "o" | x <- inputs]

This expression grabs each element from `inputs` and names it `x` (the `x <- inputs`, pronounced "`x` is drawn from `inputs`"), then applies the expression  `"_build" </> x -<.> "o"` to each element. If we start with the list `["main.c","constants.c"]`, we would end up with `["_build/main.o", "_build/constants.o"]`.

List expressions also allow us to filter the list, for example we could know that the file `"evil.c"` is in the directory, but should not be compiled. We can extend that to:

    ["_build" </> x -<.> "o" | x <- inputs, x /= "evil.c"]

The `/=` operator checks for inequality, and any predicate after the drawn from is used to first restrict which elements of the list are available.

#### Using `gcc` to collect headers

One common problem when building `.c` files is tracking down which headers they transitively import, and thus must be added as a dependency. We can solve this problem by asking `gcc` to create a file while building that contains a list of all the imports. If we run:

    gcc -c main.c -o main.o -MMD -MF main.m

That will compile `main.c` to `main.o`, and also produce a file `main.m` containing the dependencies. To add these dependencies as dependencies of this rule we can call:

    neededMakefileDependencies "main.m"

Now, if either `main.c` or any headers transitively imported by `main.c` change, the file will be rebuilt. In the initial example the complete rule is:

    "_build//*.o" %> \out -> do
        let c = dropDirectory1 $ out -<.> "c"
        let m = out -<.> "m"
        cmd_ "gcc -c" [c] "-o" [out] "-MMD -MF" [m]
        neededMakefileDependencies m

We first compute the source file `c` (e.g. `"main.c"`) that is associated with the `out` file (e.g. `"_build/main.o"`). We then compute a temporary file `m` to write the dependencies to (e.g. `"_build/main.m"`). We then call `gcc` using the `-MMD -MF` flags and then finally call `neededMakefileDependencies`.

#### Top-level variables

Variables local to a rule are defined using `let`, but you can also define top-level variables. Top-level variables are defined before the `main` call, for example:

    buildDir = "_build"

You can now use `buildDir` in place of `"_build"` throughout. You can also define parametrised variables (functions) by adding argument names:

    buildDir x = "_build" </> x

We can now write:

    buildDir ("run" <.> exe) %> \out -> do
        ...

All top-level variables and functions can be thought of as being expanded wherever they are used, although in practice may have their evaluation shared.

#### A clean command

A standard clean command is defined as:

    phony "clean" $ do
        putInfo "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

Running the build system with the `clean` argument, e.g. `runhaskell Shakefile.hs clean` will remove all files under the `_build` directory. This clean command is formed from two separate pieces. Firstly, we can define `phony` commands as:

<pre>
phony "<i>name</i>" $ do
    <i>actions</i>
</pre>

Where <tt><i>name</i></tt> is the name used on the command line to invoke the actions, and <tt><i>actions</i></tt> are the list of things to do in response. These names are not dependency tracked and are run afresh each time they are requested.

The <tt><i>actions</i></tt> can be any standard build actions, although for a `clean` rule, `removeFilesAfter` is typical. This function waits until after any files have finished building (which will be none, if you do `runhaskell Shakefile.hs clean`) then deletes all files matching `//*` in the `_build` directory. The `putInfo` function writes out a message to the console, as long as `--quiet` was not passed.

## Running

This section covers how to run the build system you have written.

#### Compiling the build system

As shown before, we can use `runhaskell Shakefile.hs` to execute our build system, but doing so causes the build script to be compiled afresh each time. A more common approach is to add a shell script that compiles the build system and runs it. In the example directory you will find `build.sh` (Linux) and `build.bat` (Windows), both of which execute the same interesting commands. Looking at `build.sh`:

    #!/bin/sh
    mkdir -p _shake
    ghc --make Shakefile.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake/build "$@"

This script creates a folder named `_shake` for the build system objects to live in, then runs `ghc --make Shakefile.hs` to produce `_shake/build`, then executes `_shake/build` with all arguments it was given. The `-with-rtsopts` flag instructs the Haskell compiler to disable "idle garbage collection", making more CPU available for the commands you are running, as [explained here](https://stackoverflow.com/questions/34588057/why-does-shake-recommend-disabling-idle-garbage-collection/).

Now you can run a build by typing `stack exec ./build.sh` on Linux, or `stack exec build.bat` on Windows. On Linux you may want to alias `build` to `stack exec ./build.sh`. For the rest of this document we will assume `build` runs the build system.

_Warning:_ You should not use the `-threaded` for GHC 7.6 or below because of a [GHC bug](https://ghc.haskell.org/trac/ghc/ticket/7646). If you do turn on `-threaded`, you should include `-qg` in `-with-rtsopts`.

#### Command line flags

The initial example build system supports a number of command line flags, including:

* `build` will compile all files required by `want`.
* `build _build/main.o` will compile enough to create `_build/main.o`, ignoring all `want` requirements.
* `build clean` will delete the contents of `_build`, because of our `phony` command.
* `build --help` will list out all flags supported by the build system, currently 36 flags. Most flags supported by `make` are also supported by Shake based build systems.
* `build -j8` will compile up to 8 rules simultaneously, by default Shake uses 1 processor.

Most flags can also be set within the program by modifying the `shakeOptions` value. As an example, `build --metadata=_metadata` causes all Shake metadata files to be stored with names such as `_metadata/.shake.database`. Alternatively we can write `shakeOptions{shakeFiles="_metadata"}` instead of our existing `shakeFiles="_build"`. Values passed on the command line take preference over those given by `shakeOptions`. Multiple overrides can be given to `shakeOptions` by separating them with a comma, for example `shakeOptions{shakeFiles="_build", shakeThreads=8}`.

<span class="target" id="progress"></span>

#### Progress prediction

One useful feature of Shake is that it can predict the remaining build time, based on how long previous builds have taken. The number is only a prediction, but it does take account of which files require rebuilding, how fast your machine is currently running, parallelism settings etc. You can display progress messages in the titlebar of a Window by either:

* Running `build --progress`
* Setting `shakeOptions{shakeProgress = progressSimple}`

The progress message will be displayed in the titlebar of the window, for example `3m12s (82%)` to indicate that the build is 82% complete and is predicted to take a further 3 minutes and 12 seconds. If you are running Windows 7 or higher and place the [`shake-progress`](https://github.com/ndmitchell/shake/releases/tag/shake-progress-1) utility somewhere on your `%PATH%` then the progress will also be displayed in the taskbar progress indicator:

![](shake-progress.png)

Progress prediction is likely to be relatively poor during the first build and after running `build clean`, as then Shake has no information about the predicted execution time for each rule. To rebuild from scratch without running clean (because you really want to see the progress bar!) you can use the argument `--always-make`, which assumes all rules need rerunning.

<span class="target" id="lint"></span>

#### Lint

Shake features a built in "lint" feature to check the build system is well formed. To run use `build --lint`. You are likely to catch more lint violations if you first `build clean`. Sadly, lint does _not_ catch missing dependencies. However, it does catch:

* Changing the current directory, typically with `setCurrentDirectory`. You should never change the current directory within the build system as multiple rules running at the same time share the current directory. You can still run `cmd_` calls in different directories using the `Cwd` argument.
* Outputs that change after Shake has built them. The usual cause of this error is if the rule for `foo` also writes to the file `bar`, despite `bar` having a different rule producing it.

There is a performance penalty for building with `--lint`, but it is typically small. For more details see [this page on all the Lint options](Lint.md).

<span class="target" id="profiling"></span>

#### Profiling and optimisation

Shake features an advanced profiling feature. To build with profiling run `build --report`, which will generate an interactive HTML profile named `report.html`. This report lets you examine what happened in that run, what takes most time to run, what rules depend on what etc. For a full explanation of how to profile and optimise a build system, including getting accurate timings and using Haskell profiling, see [the profiling and optimisation page](Profiling.md).

#### Tracing and debugging

To debug a build system there are a variety of techniques that can be used:

* Run with lint checking enabled (`--lint`), which may spot and describe the problem for you.
* Run in single-threaded mode (`-j1`) to make any output clearer by not interleaving commands.
* By default a Shake build system prints out a message every time it runs a command. Use verbose mode (`--verbose`) to print more information to the screen, such as which rule is being run. Additional `--verbose` flags increase the verbosity. Three verbosity flags produce output intended for someone debugging the Shake library itself, rather than a build system based on it.
* To raise a build error call `error "error message"`. Shake will abort, showing the error message.
* To output additional information use `putInfo "output message"`. This message will be printed to the console when it is reached.
* To show additional information with either `error` or `putInfo`, use `error $ show ("message", myVariable)`. This allows you to show any local variables.

## Extensions

This section details a number of build system features that are useful in some build systems, but not the initial example, and not most average build systems.

#### Advanced `cmd` usage

The `cmd_` has a related function `cmd` that can also obtain the stdout and stderr streams, along with the  exit code. As an example:

    (Exit code, Stdout out, Stderr err) <- cmd "gcc --version"

Now the variable `code` is bound to the exit code, while `out` and `err` are bound to the stdout and stderr streams. If `ExitCode` is not requested then any non-zero return value will raise an error.

Both `cmd_` and `cmd` also take additional parameters to control how the command is run. As an example:

    cmd_ Shell (Cwd "temp") "pwd"

This runs the `pwd` command through the system shell, after first changing to the `temp` directory.

#### Dependencies on environment variables

You can use tracked dependencies on environment variables using the `getEnv` function. As an example:

    link <- getEnv "C_LINK_FLAGS"
    let linkFlags = fromMaybe "" link
    cmd_ "gcc -o" [output] inputs linkFlags

This example gets the `$C_LINK_FLAGS` environment variable (which is `Maybe String`, namely a `String` that might be missing), then using `fromMaybe` defines a local variable `linkFlags` that is the empty string when `$C_LINK_FLAGS` is not set. It then passes these flags to `gcc`.

If the `$C_LINK_FLAGS` environment variable changes then this rule will rebuild.

#### Dependencies on extra information

Using Shake we can depend on arbitrary extra information, such as the version of `gcc`, allowing us to automatically rebuild all C files when a different compiler is placed on the path. To track the version, we can define a rule for the file `gcc.version` which changes only when `gcc --version` changes:

    "gcc.version" %> \out -> do
        alwaysRerun
        Stdout stdout <- cmd "gcc --version"
        writeFileChanged out stdout

This rule has the action `alwaysRerun` meaning it will be run in every execution that requires it, so the `gcc --version` is always checked. This rule defines no dependencies (no `need` actions), so if it lacked `alwaysRerun`, this rule would only be run when `gcc.version` was missing. The function then runs `gcc --version` storing the output in `stdout`. Finally, it calls `writeFileChanged` which writes `stdout` to `out`, but only if the contents have changed. The use of `writeFileChanged` is important otherwise `gcc.version` would change in every run. To use this rule, we `need ["gcc.version"]` in every rule that calls `gcc`.

Shake also contains a feature called "oracles", which lets you do the same thing without the use of a file, which is sometimes more convenient. Interested readers should look at the function documentation list for `addOracle`.

#### Resources

Resources allow us to limit the number of simultaneous operations more precisely than just the number of simultaneous jobs (the `-j` flag). For example, calls to compilers are usually CPU bound but calls to linkers are usually disk bound. Running 8 linkers will often cause an 8 CPU system to grid to a halt. We can limit ourselves to 4 linkers with:

    disk <- newResource "Disk" 4
    want [show i <.> "exe" | i <- [1..100]]
    "*.exe" %> \out -> do
        withResource disk 1 $ do
            cmd_ "ld -o" [out] ...
    "*.o" %> \out -> do
        cmd_ "cl -o" [out] ...

Assuming `-j8`, this allows up to 8 compilers, but only a maximum of 4 linkers.

#### Multiple outputs

Some tools, for example [bison](https://www.gnu.org/software/bison/), can generate multiple outputs from one execution. We can track these in Shake using the `&%>` operator to define rules:

    ["//*.bison.h","//*.bison.c"] &%> \[outh, outc] -> do
        let src = outc -<.> "y"
        cmd_ "bison -d -o" [outc] [src]

Now we define a list of patterns that are matched, and get a list of output files. If any output file is required, then all output files will be built, with proper dependencies.

#### Changing build rules

Shake build systems are set up to rebuild files when the dependencies change, but mostly assume that the build rules themselves do not change (including both the code and the shell commands contained within). To minimise the impact of build rule changes there are three approaches:

_Use configuration files:_ Most build information, such as which files a C file includes, can be computed from source files. Where such information is not available, such as which C files should be linked together to form an executable, use configuration files to provide the information. The rule for linking can use these configuration files, which can be properly tracked. Moving any regularly changing configuration into separate files will significantly reduce the number of build system changes.

_Depend on the build source:_ One approach is to depend on the build system source in each of the rules, then if _any_ rules change, _everything_ will rebuild. While this option is safe, it may cause a significant number of redundant rebuilds. As a restricted version of this technique, for a generated file you can include a dependency on the generator source and use `writeFileChanged`. If the generator changes it will rerun, but typically only a few generated files will change, so little is rebuilt.

_Use a version stamp:_ There is a field named `shakeVersion` in the `ShakeOptions` record. If the build system changes in a significant and incompatible way, you can change this field to force a full rebuild. If you want all rules to depend on all rules, you can put a hash of the build system source in the version field, as [described here](https://stackoverflow.com/questions/18532552/shake-how-to-reliably-automatically-force-rebuild-when-my-rules-change-becomi/18532553#18532553).

## The Haskell Zone

From now on, this manual assumes some moderate knowledge of Haskell. Most of the things in this section are either impossible to do with other build systems or can be faked by shell script. None of the Haskell is particularly advanced.

#### Haskell Expressions

You can use any Haskell function at any point. For example, to only link files without numbers in them, we can `import Data.Char` and then write:

    let os = ["_build" </> c -<.> "o" | c <- inputs, not $ any isDigit c]

For defining non-overlapping rules it is sometimes useful to use a more advanced predicate. For example, to define a rule that only builds results which have a numeric extension, we can use the `?>` rule definition function:

    (\x -> all isDigit $ drop 1 $ takeExtension x) ?> \out -> do
        ...

We first get the extension with `takeExtension`, then use `drop 1` to remove the leading `.` that `takeExtension` includes, then test that all the characters are numeric.

The standard `%>` operator is actually defined as:

    pattern %> actions = (pattern ?==) ?> actions

Where `?==` is a function for matching file patterns.

#### Haskell Actions

You can run any Haskell `IO` action by using `liftIO`. As an example:

    liftIO $ launchMissiles True

Most common IO operations to run as actions are already wrapped and available in the Shake library, including `readFile'`, `writeFile'` and `copyFile'`. Other useful functions can be found in `System.Directory`.
