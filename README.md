Shaking up GHC
==============

As part of my 6-month research secondment to Microsoft Research in Cambridge I am taking up the challenge of migrating the current [GHC](https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler) build system based on standard `make` into a new and (hopefully) better one based on [Shake](https://github.com/ndmitchell/shake/blob/master/README.md). If you are curious about the project you can find more details on the [wiki page](https://ghc.haskell.org/trac/ghc/wiki/Building/Shake) and in this [blog post](https://blogs.ncl.ac.uk/andreymokhov/shaking-up-ghc/).

This is supposed to go into the `shake-build` directory of the GHC source tree.

Trying it
---------

On Linux,
```
$ git clone git://git.haskell.org/ghc
$ cd ghc
$ git submodule update --init
$ git clone git://github.com/snowleopard/shaking-up-ghc shake-build
$ ./boot
$ ./configure
$ make inplace/bin/ghc-cabal   # This needs to be fixed
$ shake-build/build.sh
```

On Windows,
```
$ git clone --recursive git://git.haskell.org/ghc.git
$ cd ghc
$ git clone git://github.com/snowleopard/shaking-up-ghc shake-build
$ ./boot
$ ./configure --enable-tarballs-autodownload
$ make inplace/bin/ghc-cabal   # This needs to be fixed
$ shake-build/build.bat
```
Also see the Building GHC on Windows guide: https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Windows.

How to contribute
-----------------
The best way to contribute is to try the new build system, report the issues you found, and attempt to fix them. Please note the codebase is very unstable at present and we expect a lot of further refactoring. Before attempting to fix any issue do make sure no one else is already working on it. The documentation is currently non-existent, but we will start addressing this once the codebase stabilises.
