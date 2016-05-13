Hadrian
=======

[![Linux & OS X status](https://img.shields.io/travis/snowleopard/hadrian/master.svg?label=Linux%20%26%20OS%20X)](https://travis-ci.org/snowleopard/hadrian) [![Windows status](https://img.shields.io/appveyor/ci/snowleopard/hadrian/master.svg?label=Windows)](https://ci.appveyor.com/project/snowleopard/hadrian)

Hadrian is a new build system for the [Glasgow Haskell Compiler][ghc]. It is based
on [Shake][shake] and we hope that it will eventually replace the current
[`make`-based build system][make]. If you are curious about the rationale and initial
ideas behind the project you can find more details on the [wiki page][ghc-shake-wiki]
and in this [blog post][blog-post-1]. This project was formerly known as *Shaking-up-GHC*.

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
packages, which need to be installed: `ansi-terminal`, `mtl`, `shake`, `QuickCheck`.

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
Once your first build is successful, simply run `build` to rebuild. Most build artefacts
are placed into `_build` and `inplace` directories ([#113][build-artefacts-issue]).

#### Command line flags

In addition to standard Shake flags (try `--help`), the build system
currently supports several others:
* `--flavour=FLAVOUR`: choose a build flavour. Two settings are currently supported:
`default` and `quick` (adds `-O0` flag to all GHC invocations and disables library
profiling, which speeds up builds by 3-4x).

* `--haddock`: build Haddock documentation.

* `--progress-info=STYLE`: choose how build progress info is printed. There are four
settings: `none`, `brief` (one line per build command), `normal` (typically a box per
build command; this is the default setting), and `unicorn` (when `normal` just won't do).

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

#### User settings

The `make`-based build system uses `mk/build.mk` to specify user build settings. We
use [`src/Settings/User.hs`][user-settings] for the same purpose. Feel free to
experiment following the Haddock comments.

#### Clean and full rebuild

* `build clean` removes all build artefacts. Note, we are working towards a
complete separation of GHC sources and build artefacts: [#113][build-artefacts-issue].

* `build -B` forces Shake to rerun all rules, even if results of the previous build
are still in the GHC tree. 

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
* We only build `vanilla` and `profiling` way: [#4][dynamic-issue].
* Validation is not implemented: [#187][validation-issue].
* Only HTML Haddock documentation is supported (use `--haddock` flag). 
* Build flavours and conventional command line flags are not implemented: [#188][flavours-issue].
* Cross-compilation is not implemented: [#177][cross-compilation-issue].
* There is no support for installation or binary/source distribution: [#219][install-issue].

Check out [milestones] to see when we hope to resolve the above limitations.

How to contribute
-----------------

The best way to contribute is to try the new build system, report the issues
you found, and attempt to fix them. Please note the codebase is very unstable
at present and we expect a lot of further refactoring. The documentation is
currently non-existent, but we are working on it: [#55][comments-issue],
[#56][doc-issue]. See also [#239](https://github.com/snowleopard/hadrian/issues/239)
for a list of issues on the critical path.

Acknowledgements
----------------

I started this project as part of my 6-month research visit to Microsoft
Research Cambridge, which was funded by Newcastle University, EPSRC, and
Microsoft Research. I would like to thank Simon Peyton Jones, Neil Mitchell
and Simon Marlow for kick-starting the project and for their guidance. Last
but not least, big thanks to the project [contributors][contributors], who
helped me endure and enjoy the project.

[ghc]: https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler
[shake]: https://github.com/ndmitchell/shake/blob/master/README.md
[make]: https://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
[ghc-shake-wiki]: https://ghc.haskell.org/trac/ghc/wiki/Building/Shake
[blog-post-1]: https://blogs.ncl.ac.uk/andreymokhov/shaking-up-ghc
[issues]: https://github.com/snowleopard/hadrian/issues
[ghc-preparation]: https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation
[ghc-windows-quick-build]: https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Windows#AQuickBuild
[windows-build]: https://github.com/snowleopard/hadrian/blob/master/doc/windows.md
[build-artefacts-issue]: https://github.com/snowleopard/hadrian/issues/113
[ghc-split-objs-bug]: https://ghc.haskell.org/trac/ghc/ticket/11315
[user-settings]: https://github.com/snowleopard/hadrian/blob/master/src/Settings/User.hs
[test-issue]: https://github.com/snowleopard/hadrian/issues/197
[dynamic-issue]: https://github.com/snowleopard/hadrian/issues/4
[validation-issue]: https://github.com/snowleopard/hadrian/issues/187
[flavours-issue]: https://github.com/snowleopard/hadrian/issues/188
[cross-compilation-issue]: https://github.com/snowleopard/hadrian/issues/177
[install-issue]: https://github.com/snowleopard/hadrian/issues/219
[milestones]: https://github.com/snowleopard/hadrian/milestones
[comments-issue]: https://github.com/snowleopard/hadrian/issues/55
[doc-issue]: https://github.com/snowleopard/hadrian/issues/56
[contributors]: https://github.com/snowleopard/hadrian/graphs/contributors
