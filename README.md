Shaking up GHC
==============

[![Linux & OS X status](https://img.shields.io/travis/snowleopard/shaking-up-ghc/master.svg?label=Linux%20%26%20OS%20X)](https://travis-ci.org/snowleopard/shaking-up-ghc) [![Windows status](https://img.shields.io/appveyor/ci/snowleopard/shaking-up-ghc/master.svg?label=Windows)](https://ci.appveyor.com/project/snowleopard/shaking-up-ghc)

This is a new build system for the [Glasgow Haskell Compiler][ghc]. It is based
on [Shake][shake] and we hope that it will eventually replace the current
[`make`-based build system][make]. If you are curious about the rationale and initial
ideas behind the project you can find more details on the [wiki page][ghc-shake-wiki]
and in this [blog post][blog-post-1].

The new build system can work side-by-side with the existing build system. Note, there is
some interaction between them: they put (some) build results in the same directories,
e.g. `inplace/bin/ghc-stage1`. 

[Join us on #shaking-up-ghc on Freenode](irc://chat.freenode.net/#shaking-up-ghc).

Your first build
----------------

Beware, the build system is in the alpha development phase. Things are shaky and often
break; there are numerous [known issues][issues]. Not afraid? Then put on the helmet and
follow these steps:

* If you have never built GHC before, start with the [preparation guide][ghc-preparation].

* This build system is written in Haskell (obviously) and depends on the following Haskell
packages, which need to be installed: `ansi-terminal`, `mtl`, `shake`, `QuickCheck`.

* Get the sources. It is important for the build system to be in the `shake-build` directory of the GHC source tree:

    ```bash
    git clone --recursive git://git.haskell.org/ghc.git
    cd ghc
    git clone git://github.com/snowleopard/shaking-up-ghc shake-build
    ```
* Start your first build (you might want to enable parallelism with `-j`):

    ```bash
    shake-build/build.sh --configure
    ```
On Windows, use `build.bat` instead and pass an extra flag to configure (also see [building on Windows][ghc-windows-quick-build]):
    ```bash
    shake-build/build.bat --configure=--enable-tarballs-autodownload
    ```
If you are interested in building in a Cabal sandbox or using Stack, have a look at `shake-build/build.cabal.sh` and `shake-build/build.stack.sh` scripts.

Using the build system
----------------------
Once your first build is successful, simply run `shake-build/build.sh` or `shake-build/build.bat`
to rebuild (you no longer need to use the `--configure` flag). Most build artefacts are placed
into `.build` and `inplace` directories ([#113][build-artefacts-issue]).

#### Command line flags

In addition to standard Shake flags (try `--help`), the build system
currently supports several others:
* `--configure[=ARGS]`: run the `configure` script forwarding `ARGS` as command line
arguments; also run the `boot` script to create the `configure` script if necessary.
You do not have to use this functionality of the new build system; feel free to run
`boot` and `configure` scripts manually, as you do when using `make`.
* `--flavour=FLAVOUR`: choose a build flavour. Two settings are currently supported:
`default` and `quick` (adds `-O0` flag to all GHC invocations speeding up builds by 3x).
* `--progress-info=STYLE`: choose how build progress info is printed. There are four
settings: `none`, `brief` (one line per build command), `normal` (typically a box per
build command; this is the default setting), and `unicorn` (when `normal` just won't do).
* `--split-objects`: generate split objects, which are switched off by default. Due to
a GHC [bug][ghc-split-objs-bug], you need a full clean rebuild when using this flag.

#### User settings

The `make`-based build system uses `mk/build.mk` to specify user build settings. We
use [`src/Settings/User.hs`][user-settings] for the same purpose. Feel free to
experiment following the Haddock comments.

#### Clean and full rebuild

* `shake-build/build.sh clean` removes all build artefacts. Note, we are working towards a complete separation of GHC sources and build artefacts: [#113][build-artefacts-issue].

* `shake-build/build.sh -B` forces Shake to rerun all rules, even if results of the previous build are still in the GHC tree. 

Current limitations
-------------------
The new build system still lacks many important features:
* We only build `vanilla` way: [#4][dynamic-issue], [#186][profiling-issue].
* Documentation is broken: [#98][haddock-issue].
* Validation is not implemented: [#187][validation-issue].
* Build flavours and conventional command line flags are not implemented: [#188][flavours-issue].
* Cross-compilation is not implemented: [#177][cross-compilation-issue].

Check out [milestones] to see when we hope to resolve the above limitations.

How to contribute
-----------------

The best way to contribute is to try the new build system, report the issues
you found, and attempt to fix them. Please note the codebase is very unstable
at present and we expect a lot of further refactoring. The documentation is
currently non-existent, but we are working on it: [#55][comments-issue],
[#56][doc-issue].

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
[issues]: https://github.com/snowleopard/shaking-up-ghc/issues
[ghc-preparation]: https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation
[ghc-windows-quick-build]: https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Windows#AQuickBuild
[build-artefacts-issue]: https://github.com/snowleopard/shaking-up-ghc/issues/113
[ghc-split-objs-bug]: https://ghc.haskell.org/trac/ghc/ticket/11315
[user-settings]: https://github.com/snowleopard/shaking-up-ghc/blob/master/src/Settings/User.hs
[dynamic-issue]: https://github.com/snowleopard/shaking-up-ghc/issues/4
[profiling-issue]: https://github.com/snowleopard/shaking-up-ghc/issues/186
[haddock-issue]: https://github.com/snowleopard/shaking-up-ghc/issues/98
[validation-issue]: https://github.com/snowleopard/shaking-up-ghc/issues/187
[flavours-issue]: https://github.com/snowleopard/shaking-up-ghc/issues/188
[cross-compilation-issue]: https://github.com/snowleopard/shaking-up-ghc/issues/177
[milestones]: https://github.com/snowleopard/shaking-up-ghc/milestones
[comments-issue]: https://github.com/snowleopard/shaking-up-ghc/issues/55
[doc-issue]: https://github.com/snowleopard/shaking-up-ghc/issues/56
[contributors]: https://github.com/snowleopard/shaking-up-ghc/graphs/contributors
