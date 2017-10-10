Hadrian
=======

[![Linux & OS X status](https://img.shields.io/travis/snowleopard/hadrian/master.svg?label=Linux%20%26%20OS%20X)](https://travis-ci.org/snowleopard/hadrian) [![Windows status](https://img.shields.io/appveyor/ci/snowleopard/hadrian/master.svg?label=Windows)](https://ci.appveyor.com/project/snowleopard/hadrian) [![OS X status](https://img.shields.io/circleci/project/github/snowleopard/hadrian.svg?label=OS%20X)](https://circleci.com/gh/snowleopard/hadrian)

Hadrian is a new build system for the [Glasgow Haskell Compiler][ghc]. It is based
on [Shake][shake] and we hope that it will eventually replace the current
[`make`-based build system][make]. If you are curious about the rationale behind the
project and the architecture of the build system you can find more details in
this [Haskell Symposium 2016 paper][paper] and this [Haskell eXchange 2016 talk][talk].

The new build system can work side-by-side with the existing build system. Note, there is
some interaction between them: they put (some) build results in the same directories,
e.g. `inplace/bin/ghc-stage1`.

Your first build
----------------

Beware, the build system is in the alpha development phase. Things are shaky and often
break; there are numerous [known issues][issues]. Not afraid? Then put on the helmet and
follow these steps:

* If you have never built GHC before, start with the [preparation guide][ghc-preparation].

* This build system is written in Haskell (obviously) and depends on the following Haskell
packages, which need to be installed: `ansi-terminal extra mtl quickcheck shake`.

* Get the sources. It is important for the build system to be in the `hadrian` directory
of the GHC source tree:

    ```bash
    git clone --recursive git://git.haskell.org/ghc.git
    cd ghc
    git clone git://github.com/snowleopard/hadrian
    ```

* Build GHC using `hadrian/build.sh` or `hadrian/build.bat` (on Windows) instead
of `make`. You might want to enable parallelism with `-j`. We will further refer to the
build script simply as `build`. If you are interested in building in a Cabal sandbox
or using Stack, have a look at `build.cabal.sh` and `build.stack.sh` scripts. Also
see [instructions for building GHC on Windows using Stack][windows-build]. Note, Hadrian
runs the `boot` and `configure` scripts automatically on the first build, so that you don't
need to. Use `--skip-configure` to suppress this behaviour (see overview of command line
flags below).

Using the build system
----------------------
Once your first build is successful, simply run `build` to rebuild. Build results
are placed into `_build` and `inplace` directories.

#### Command line flags

In addition to standard Shake flags (try `--help`), the build system
currently supports several others:
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

* `--haddock`: build Haddock documentation.

* `--integer-simple`: build GHC using the `integer-simple` integer library (instead
of `integer-gmp`).

* `--progress-colour=MODE`: choose whether to use colours when printing build progress
info. There are three settings: `never` (do not use colours), `auto` (attempt to detect
whether the console supports colours; this is the default setting), and `always` (use
colours).

* `--progress-info=STYLE`: choose how build progress info is printed. There are four
settings: `none`, `brief` (one line per build command; this is the default setting),
`normal` (typically a box per build command), and `unicorn` (when `normal` just won't do).

* `--skip-configure`: use this flag to suppress the default behaviour of Hadrian that
runs the `boot` and `configure` scripts automatically if need be, so that you don't have
to remember to run them manually. With `--skip-configure` you will need to manually run:

    ```bash
    ./boot
    ./configure # On Windows run ./configure --enable-tarballs-autodownload
    ```
as you normally do when using `make`. Beware, by default Hadrian may do network I/O on
Windows to download necessary tarballs, which may sometimes be undesirable; `--skip-configure`
is your friend in such cases.

* `--split-objects`: generate split objects, which are switched off by default. Due to
a GHC [bug][ghc-split-objs-bug], you need a full clean rebuild when using this flag.

* `--verbose`: run Hadrian in verbose mode. In particular this prints diagnostic messages
by Shake oracles.

#### User settings

The `make`-based build system uses `mk/build.mk` to specify user build settings. We
use `hadrian/UserSettings.hs` for the same purpose, see [documentation](doc/user-settings.md).

#### Clean and full rebuild

* `build clean` removes all build artefacts.

* `build -B` forces Shake to rerun all rules, even if the previous build results are
are still up-to-date.

#### Source distribution

To build a GHC source distribution tarball, run Hadrian with the `sdist-ghc` target.

#### Installation

To build and install GHC artifacts, run the `install` target.

By default, the artifacts will be installed to `<prefix>` on your system
(in this case, the `DESTDIR` is empty, corresponds to the root of the file system).
For example on UNIX, `ghc` will be installed to `/usr/local/bin`. By setting flag `--install-destdir=[DESTDIR]`,
you can install things to non-system path `DESTDIR/<prefix>` instead.
Make sure you use correct absolute path on Windows, e.g. `C:/path`,
i.e. GHC is installed into `C:/path/usr/local` for the above example.

#### Testing

* `build validate` runs GHC tests by simply executing `make fast` in `testsuite/tests`
directory. This can be used instead of `sh validate --fast --no-clean` in the existing
build system. Note: this will rebuild Stage2 GHC, `ghc-pkg` and `hpc` if they are out of date.

* `build test` runs GHC tests by calling the `testsuite/driver/runtests.py` python
script with appropriate flags. The current implementation is limited and cannot
replace the `validate` script (see [#187][validation-issue]).

* `build selftest` runs tests of the build system. Current test coverage is close to
zero (see [#197][test-issue]).

Current limitations
-------------------
The new build system still lacks many important features:
* Validation is not implemented: [#187][validation-issue].
* Dynamic linking on Windows is not supported [#343][dynamic-windows-issue].
* Only HTML Haddock documentation is supported (use `--haddock` flag).
* Cross-compilation is not implemented: [#177][cross-compilation-issue].
* There is no support for binary distribution: [#219][install-issue].

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
[shake]: https://github.com/ndmitchell/shake/blob/master/README.md
[make]: https://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
[paper]: https://www.staff.ncl.ac.uk/andrey.mokhov/Hadrian.pdf
[talk]: https://skillsmatter.com/skillscasts/8722-meet-hadrian-a-new-build-system-for-ghc
[issues]: https://github.com/snowleopard/hadrian/issues
[ghc-preparation]: https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation
[ghc-windows-quick-build]: https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Windows#AQuickBuild
[windows-build]: https://github.com/snowleopard/hadrian/blob/master/doc/windows.md
[ghc-split-objs-bug]: https://ghc.haskell.org/trac/ghc/ticket/11315
[test-issue]: https://github.com/snowleopard/hadrian/issues/197
[validation-issue]: https://github.com/snowleopard/hadrian/issues/187
[dynamic-windows-issue]: https://github.com/snowleopard/hadrian/issues/343
[cross-compilation-issue]: https://github.com/snowleopard/hadrian/issues/177
[install-issue]: https://github.com/snowleopard/hadrian/issues/219
[milestones]: https://github.com/snowleopard/hadrian/milestones
[contributors]: https://github.com/snowleopard/hadrian/graphs/contributors
