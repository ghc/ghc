Hadrian
=======

[![Linux & OS X status](https://img.shields.io/travis/snowleopard/hadrian/master.svg?label=Linux%20%26%20OS%20X)](https://travis-ci.org/snowleopard/hadrian) [![Windows status](https://img.shields.io/appveyor/ci/snowleopard/hadrian/master.svg?label=Windows)](https://ci.appveyor.com/project/snowleopard/hadrian) [![OS X status](https://img.shields.io/circleci/project/github/snowleopard/hadrian.svg?label=OS%20X)](https://circleci.com/gh/snowleopard/hadrian)

Hadrian is a new build system for the [Glasgow Haskell Compiler][ghc]. It is based
on [Shake][shake] and we hope that it will soon replace the current
[Make-based build system][make]. If you are curious about the rationale behind the
project and the architecture of the build system you can find more details in
this [Haskell Symposium 2016 paper](https://dl.acm.org/authorize?N41275) and this
[Haskell eXchange 2016 talk][talk].

The new build system can work side-by-side with the existing build system, since it
places all build artefacts in a dedicated directory (called `_build` by default).
See [this guide](https://ghc.haskell.org/trac/ghc/wiki/Building/Hadrian/QuickStart)
if you'd like to start using Hadrian for building GHC.

Your first build
----------------

Hadrian has not entirely caught up with the make build system. Not afraid?
Then put on the helmet and run the following commands from the root of the GHC
tree:

```
./boot && ./configure
hadrian/build.sh -j
```

or on Windows:

```
./boot && ./configure --enable-tarballs-autodownload
hadrian/build.bat -j
```

Here flag `-j` enables parallelism and is optional. We will further refer to the
build script simply as `build`. Note that Hadrian can also run the `boot` and
`configure` scripts automatically for you if you pass the flag `--configure`,
or simply `-c`. See the overview of command line flags below.

Notes:

* If the default build script doesn't work, you might want to give a try to
another one, e.g. based on Cabal sandboxes (`build.cabal.*`) or
Stack (`build.stack.*`). Also see
[instructions for building GHC on Windows using Stack][windows-build].


* Hadrian is written in Haskell and depends on `shake` (plus a few packages that
`shake` depends on), `mtl`, `quickcheck`, and GHC core libraries.

* If you have never built GHC before, start with the
[preparation guide][ghc-preparation].

Using the build system
----------------------
Once your first build is successful, simply run `build` to rebuild after some
changes. Build results are placed into `_build` by default.

#### Command line flags

In addition to standard Shake flags (try `--help`), the build system
currently supports several others:

* `--build-root=PATH` or `-oPATH`: specify the directory in which you want to
store all build products. By default Hadrian builds everything in the `_build/`
subdirectory of the GHC source tree. Unlike the Make build system, Hadrian
doesn't have any "inplace" logic left anymore. This option is therefore useful
for GHC developers who want to build GHC in different ways or at different
commits, from the same source directory, and have the build products sit in
different, isolated folders.

* `--configure` or `-c`: use this flag to run the `boot` and `configure` scripts
automatically, so that you don't have to remember to run them manually as you
normally do when using Make (typically only in the first build):
    ```bash
    ./boot
    ./configure # On Windows run ./configure --enable-tarballs-autodownload
    ```
    Beware that with this flag Hadrian may do network I/O on Windows to download necessary
    tarballs, which may sometimes be undesirable.

* `--flavour=FLAVOUR`: choose a build flavour. The following settings are currently supported:
`default`, `quick`, `quickest`, `perf`, `prof`, `devel1` and `devel2`. As an example, the
`quickest` flavour adds `-O0` flag to all GHC invocations and builds libraries only in the
`vanilla` way, which speeds up builds by 3-4x. Build flavours are documented
[here](https://github.com/snowleopard/hadrian/blob/master/doc/flavours.md).

* `--freeze1`: freeze Stage1 GHC, i.e. do not rebuild it even if some of its source files
are out-of-date. This allows to significantly reduce the rebuild time when you are working
on a feature that affects both Stage1 and Stage2 compilers, but may lead to incorrect
build results. To unfreeze Stage1 GHC simply drop the `--freeze1` flag and Hadrian will
rebuild all out-of-date files.

* `--integer-simple`: build GHC using the `integer-simple` integer library (instead
of `integer-gmp`).

* `--progress-colour=MODE`: choose whether to use colours when printing build progress
info. There are three settings: `never` (do not use colours), `auto` (attempt to detect
whether the console supports colours; this is the default setting), and `always` (use
colours).

* `--progress-info=STYLE`: choose how build progress info is printed. There are four
settings: `none`, `brief` (one line per build command; this is the default setting),
`normal` (typically a box per build command), and `unicorn` (when `normal` just won't do).

* `--split-objects`: generate split objects, which are switched off by default. Due to
a GHC [bug][ghc-split-objs-bug], you need a full clean rebuild when using this flag.

* `--verbose`: run Hadrian in verbose mode. In particular this prints diagnostic messages
by Shake oracles.

#### User settings

The Make-based build system uses `mk/build.mk` to specify user build settings. We
use `hadrian/UserSettings.hs` for the same purpose, see [documentation](doc/user-settings.md).

#### Building libraries and executables

You can build a specific library or executable for a given stage by doing
`build stage<N>:<lib|exe>:<package name>`. Examples:

``` sh
# build the stage 1 GHC compiler (the executable), the binary will be placed
# under _build/stage0/bin/ghc (because it is built by the stage0 compiler).
build stage1:exe:ghc-bin

# build the stage 2 GHC compiler, the binary will be placed under
# _build/stage1/bin/ghc (because it is built by the stage1 compiler).
build stage2:exe:ghc-bin

# build the ghc library with the boot compiler, and register it
# in the package database under _build/stage0/lib.
build stage0:lib:ghc

# build the Cabal library with the stage 1 compiler and register it
# in the package database under _build/stage1/lib.

# build the text library with the stage 2 compiler and register it
# in the package database under _build/stage2/lib.
build stage2:lib:text

# build the stage 2 haddock executable and place the program under
# _build/stage1/haddock.
build stage2:exe:haddock
```

#### Testing

To run GHC's testsuite, use `build test`. See
[doc/testsuite.md](doc/testsuite.md) to learn about all the options
you can use to mimic what the Make build system offers.

`build selftest` runs tests of the build system. Current test coverage
is close to zero (see [#197][test-issue]).

#### Clean and full rebuild

* `build clean` removes all build artefacts.

* `build -B` forces Shake to rerun all rules, even if the previous build results are
are still up-to-date.

#### Documentation

To build GHC documentation, run `build docs`. Note that finer-grain documentation
targets (e.g. building only HTML documentation or only the GHC User's Guide)
are currently not supported.

#### Source distribution

To build a GHC source distribution tarball, run `build source-dist`.

#### Binary distribution

To build a GHC binary distribution, run `build binary-dist`. The resulting
tarball contains just enough to support the

``` sh
$ ./configure [--prefix=PATH] && make install
```

workflow, for now.

Troubleshooting
---------------

Here are a few simple suggestions that might help you fix the build:
  
* Hadrian is occasionally broken by changes in GHC. If this happens, you might want to switch
  to an earlier GHC commit.
  
* If Hadrian fails with the message `Configuration file hadrian/cfg/system.config is missing`,
  you have probably forgotten to pass the `--configure` flag during the first build.
  
* If you need help in debugging Hadrian, read the [wiki](https://github.com/snowleopard/hadrian/wiki)
  and Shake's [debugging tutorial](https://shakebuild.com/debugging).

If everything fails, don't hesitate to [raise an issue](https://github.com/snowleopard/hadrian/issues/new).

Current limitations
-------------------
The new build system still lacks many important features:
* Validation is not implemented: [#187][validation-issue].
* Dynamic linking on Windows is not supported [#343][dynamic-windows-issue].

Check out [milestones] to see when we hope to resolve the above limitations.

How to contribute
-----------------

The best way to contribute is to try the new build system, report the issues
you found, and attempt to fix them. Please note: the codebase is very unstable
at present and we expect a lot of further refactoring. If you would like to
work on a particular issue, please let everyone know by adding a comment about
this. The issues that are currently on the critical path and therefore require
particular attention are listed in [#239](https://github.com/snowleopard/hadrian/issues/239).
Also have a look at [projects](https://github.com/snowleopard/hadrian/projects)
where open issues and pull requests are grouped into categories.

Acknowledgements
----------------

I started this project as part of my 6-month research visit to Microsoft
Research Cambridge, which was funded by Newcastle University, EPSRC, and
Microsoft Research. I would like to thank Simon Peyton Jones, Neil Mitchell
and Simon Marlow for kick-starting the project and for their guidance.
Zhen Zhang has done fantastic work on Hadrian as part of his Summer of
Haskell 2017 [project](https://summer.haskell.org/ideas.html#hadrian-ghc),
solving a few heavy and long-overdue issues. Last but not least, big thanks
to all other project [contributors][contributors], who helped me endure and
enjoy the project.

[ghc]: https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler
[shake]: https://github.com/ndmitchell/shake
[make]: https://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
[talk]: https://skillsmatter.com/skillscasts/8722-meet-hadrian-a-new-build-system-for-ghc
[issues]: https://github.com/snowleopard/hadrian/issues
[ghc-preparation]: https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation
[ghc-windows-quick-build]: https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Windows#AQuickBuild
[windows-build]: https://github.com/snowleopard/hadrian/blob/master/doc/windows.md
[ghc-split-objs-bug]: https://ghc.haskell.org/trac/ghc/ticket/11315
[test-issue]: https://github.com/snowleopard/hadrian/issues/197
[validation-issue]: https://github.com/snowleopard/hadrian/issues/187
[dynamic-windows-issue]: https://github.com/snowleopard/hadrian/issues/343
[bin-dist-issue]: https://github.com/snowleopard/hadrian/issues/219
[milestones]: https://github.com/snowleopard/hadrian/milestones
[contributors]: https://github.com/snowleopard/hadrian/graphs/contributors
