Shaking up GHC
==============

[![Linux & OS X status](https://img.shields.io/travis/snowleopard/shaking-up-ghc/master.svg?label=Linux%20%26%20OS%20X)](https://travis-ci.org/snowleopard/shaking-up-ghc) [![Windows status](https://img.shields.io/appveyor/ci/snowleopard/shaking-up-ghc/master.svg?label=Windows)](https://ci.appveyor.com/project/snowleopard/shaking-up-ghc)


As part of my 6-month research secondment to Microsoft Research in Cambridge
I am taking up the challenge of migrating the current [GHC][ghc] build system
based on standard `make` into a new and (hopefully) better one based on
[Shake][shake]. If you are curious about the project you can find more details
on the [wiki page][ghc-shake-wiki] and in this [blog post][shake-blog-post].

This is supposed to go into the `shake-build` directory of the GHC source tree.

[Join us on #shaking-up-ghc on Freenode](irc://chat.freenode.net/#shaking-up-ghc).

Trying it
---------

Please see the [Preparation][ghc-preparation] on the GHC wiki
for general preparation. The preparation steps for the `shake` build system are
identical to those for the `make` build system. This means that you don't need
to adjust anything if you are already familiar with building ghc using the `make`
build system.

Furthermore, we depend on the following packages which need to be installed:
`ansi-terminal`, `mtl`, `shake`, `QuickCheck`.

### Getting the source and configuring GHC

```bash
git clone --recursive git://git.haskell.org/ghc.git
cd ghc
git clone git://github.com/snowleopard/shaking-up-ghc shake-build
./boot
./configure                                # on linux / os x
./configure --enable-tarballs-autodownload # on windows
```

### Configuring the build process

`ghc` uses `mk/build.mk` to configure the build process. `shaking-up-ghc`
uses `src/Settings/User.hs` for build specification.

### Building GHC using `shaking-up-ghc`

```bash
./shake-build/build.sh       # linux / os x: to run the script directly.
./shake-build/build.cabal.sh # linux / os x: OR to install the build system in a Cabal sandbox and then run it.
shake-build/build.bat        # windows
```

Also see the [Building GHC on Windows guide][ghc-windows-building-guide].

### Resetting the build

To reset the new build system run the build script with `-B` flag. This forces Shake to rerun all rules, even if results of the previous build are still in the GHC tree. This is a temporary solution; we are working on a proper reset functionality ([#131](https://github.com/snowleopard/shaking-up-ghc/issues/131)).


How to contribute
-----------------

The best way to contribute is to try the new build system, report the issues
you found, and attempt to fix them. Please note the codebase is very unstable
at present and we expect a lot of further refactoring. Before attempting to
fix any issue do make sure no one else is already working on it. The
documentation is currently non-existent, but we will start addressing this
once the codebase stabilises.


[ghc-shake-wiki]: https://ghc.haskell.org/trac/ghc/wiki/Building/Shake
[ghc-preparation]: https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation
[ghc-windows-building-guide]: https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Windows
[ghc]: https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler
[shake-blog-post]: https://blogs.ncl.ac.uk/andreymokhov/shaking-up-ghc
[shake]: https://github.com/ndmitchell/shake/blob/master/README.md
