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
  - fix many more tests, etc.

#### Template Haskell

Template Haskell is currently broken because we still need to do some restructuring to fix it.

GHCJS uses a Template Haskell runner that predates the `iserv` GHC external interpreter. The runner is mostly implemented in Haskell with a bit of JavaScript (`thrunner.js`) to deal with communication.

The runner is invoked with the `runTHServer` function, which should be in a wired-in package for consistent naming. This runner, which used to live in the `ghcjs-th` package is linked on demand with the user's code, and then incrementally with everything each new TH splice needs to be able to run. A quick solution would be to move the contents of `ghcjs-th` into an existing wired-in package like `base` or `template-haskell`, but unfortunately dependencies like `bytestring` are a problem for this. Having `runTHServer` in a different non-wired-in package causes a mess with dependencies, and breaks the way we start the runner.

A potential solution would be:

  - Instead of on-demand linking, pre-build the `runTHServer` runner and save the incremental linker state. It can be in a normal (non-wired-in) package if we make it an executable or we use `-no-hs-main` and a `foreign export` for `runTHServer`.
  - Use incremental linking to add the code for each splice. The first batch of code would contain all the foreign code for the packages in the dependencies. We may need to cache the first batch for performance like we did with the runner before.
  - GHVJS also needs to know how to (de)serialize the messages, so we still need some library that links against both the runner and the compiler.
  - Remove the `nodesettings.json` dependency, storing these settings elsewhere (like the `settings` file)

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
