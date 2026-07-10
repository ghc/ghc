# Running Ninja builds with Shake

_See also: [Shake links](https://github.com/ndmitchell/shake#readme); [Shake manual](Manual.md#readme)_

Shake supports the `.ninja` file format used by the [Ninja tool](https://ninja-build.org/). This document describes how to use Shake instead of Ninja, and why you might want to do so.

#### Installing Shake

1. Install the [Haskell Platform](https://www.haskell.org/platform/), which provides a Haskell compiler and standard libraries.
2. Type `cabal update`, to download information about the latest versions of all Haskell packages.
3. Type `cabal install shake --global`, to build and install Shake and all its dependencies.
4. Type `shake --help`, to verify that Shake was successfully installed.

#### Running Shake

Change to the directory you usually run Ninja from (where there is a `build.ninja` file) and instead of typing `ninja` type `shake`. Ninja defaults to guessing how many processors to use, while Shake defaults to only 1 processor, so you will probably want to run something like `shake -j` to use all processors or `-j4` to use 4 processors (with a number appropriate for your computer).

The following Ninja options are available in Shake:

* Print version is `--version` in Shake.
* Change directory before building is `--directory` in Shake.
* Specify the `.ninja` file is `--file` in Shake.
* Parallelism is `-j` in Shake.
* Avoiding starting new jobs if the load average is over a certain level is not currently supported in Shake.
* Keep going until a number of jobs fail is best approximated by `--keep-going` which keeps going regardless of how many jobs fail.
* Dry run is not supported in Shake.
* Show command lines while building is `--verbose` in Shake.
* Debugging stats is `--timings` in Shake.
* Debugging explanations are achieved with `--debug` (mostly for Shake developers) and `--report` for end users.
* Many of the Ninja subtools have equivalent versions inside `--report`.

#### Additional features of Shake

For people who are set up to run an existing `.ninja` build file, there are two features of Shake that may appeal:

* If you build with `--report` the file `report.html` will be generated. Open that report file and you can see numerous details about the build -- how good the parallel utilisation was, what changed to cause what to rebuild, summary statistics, a dependency graph and more. See the Help page in any generated report for more details.
* If you build with `--progress` the console titlebar will display a predicted completion time, how many seconds until your build completes. The predicted time will be fairly inaccurate the first time round, but future runs are influenced by recorded timings, and can produce useful guesses.
* If you build with `--lint` certain invariants will be checked, and an error will be raised if they are violated. For example, if you depend on a generated file via `depfile`, but do not list it as a dependency (even an order only dependency), an error will be raised. 

#### FAQ

* If I get this working, or can't get it working because of a bug, do you care? Yes -- please [raise an issue](https://github.com/ndmitchell/issues).
* Is Shake compatible with all Ninja features? Shake has support for everything in the Ninja manual -- including response files, deps, pools and restat. Shake does not yet support rebuilding a file if the command line changes (if people rely on this feature, I am happy to add it). I am unaware of any Ninja files that don't work, but would be surprised if there were not some corner cases that Shake gets wrong (but tell me, and I'll fix it).
* Is Shake faster or slower than Ninja? I have one data point -- compiling LLVM on Windows under mingw they both take the same time to compile initially, and Ninja takes 0.9s for a nothing to do build vs Shake at 0.8s. Shake is slower at parsing Ninja files, so if you have _huge_ `.ninja` files (e.g. Chromium) Shake will probably be slower. Shake does less work if you don't specify deps, which is probably why it is faster on LLVM (but you should specify deps -- it makes both Shake and Ninja faster). As people report more results I am sure both Shake and Ninja will be optimised.
* Why did you make Shake interpret `.ninja` files? There are a few reasons: 1) It seemed like fun. 2) The Ninja team have made both [CMake](https://cmake.org/) and [gyp](https://code.google.com/p/gyp/) both generate `.ninja` files, so Shake can now build far more projects. 3) Shake and Ninja are both designed to be fast, benchmarking them has already improved the speed of Shake.
