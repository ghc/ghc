# GHCJS build

This is a work in progress to build GHCJS as a regular GHC target.

### Building

  1. add emscripten toolchain to PATH (for example running the `emsdk_env.sh`)
  2. Run `./build-ghcjs.sh` script to build the compiler

This places the compiler and utilities in `./inplace/bin`

### Testing

The testsuite requires some environment variables to be set. Use the following script as a starting point:

```
#!/usr/bin/env bash

ROOT=$PWD
NODE=$HOME/emsdk/node/14.15.5_64bit/bin/node

STAGE1_GHC=$ROOT/inplace/bin/ghc-stage1
TEST_WRAPPER=$NODE
SKIP_PERF_TESTS=YES
LD=ld
TEST_HC=$ROOT/inplace/bin/ghc-stage1
THREADS=64
VERBOSE=1

make test stage=1
```


# Status

The compiler builds the boot libraries correctly and can produce working executables, but not all functionality works yet, and there are many failing tests. I haven't encountered the random failures that plague the standalone "cabal package" GHCJS.

We keep a "work list" of active items here, which should be regularly updated. We'll make tickets for items that may require discussion.

### Todo

  - remove `build-ghcjs.sh` script and integrate its functionality in build system (in particular copying the data files). Perhaps keep a script for making `js-unknown-ghcjs` toolchain wrappers.
  - remove shims functionality and add files to rts package or other packages `.a` archives
  - drop the new dependencies that GHCJS introduced by reworking the GHCJS code (new packages add code bloat and significantly slow down rebuilds)
  - build the testsuite `ghc-config.hs` with the boot compiler automatically
  - rebase 8.10 version on 8.10.7
  - update to 9.2 / 9.3 and make sure everything works with hadrian build system
  - make compiler less "different" and more flexible where possible, for example adopt a standard ld-style linker
  - fix many more tests, template haskell etc.

### Done

  - integrate GHCJS source code and temporarily add dependencies in `libraries`
  - add `JSVal` als real wired-in name and drop FFI typechecking hacks
  - add ghcjs cross target flavours
  - make `js-unknown-ghcjs` target work with the autoconf scripts
  - add GHCJS-specific patches for packages
  - fixes to building libraries with emscripten (for example `rts`, `libffi`, `libgmp`), even if we don't use the results
  - switch to standard `.o` extension (from `.js_o`)
  - use standard ar `.a` files instead of custom `.js_a`
  - handle JS sources in build system / `ghc-cabal`
  - generate usable primops and platform constants
