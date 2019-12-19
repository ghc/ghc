# Hadrian for Make users

This is a cheatsheet-style document meant to succinctly show how to use
Hadrian for any GHC developer familiar with the Make build system, by showing
the Make command(s) to achieve some goal and then the Hadrian equivalent. If you
are looking for a more verbose and comprehensive document, you should head to
[Hadrian's README](../README.md).

## tl;dr

For GHC hackers already used to the Make build system, here is what you need to
know:

- You can still boot and configure yourself.
- Use `hadrian/build.{sh, bat}` instead of `make`. It supports `-j`. This build
  script will from now on be referred to as simply `build`.
- Add the `-c` flag if you want hadrian to boot and configure the source tree
  for you.
- Build products are not in `inplace` anymore, but `_build` by default. Your
  stage 2 GHC would then be at `_build/stage1/bin/ghc` (because it's built by
  the stage 1 compiler).
- The build root is configurable with `--build-root` or `-o`.
- You can pick the build flavour with `--flavour=X` where X is `perf`, `prof`,
  etc.
- You can run tests with `build test`, and specific ones by adding
  `--only="T12345 T11223"` for example.
- GHCs built by Hadrian are relocatable on Windows, Linux, OS X and FreeBSD.
  This means you can move the `<build root>/stage1/{lib, bin}` directories
  around and GHC will still happily work, as long as both directories stay next
  to each other.

Of particular interest is the `--build-root/-o` option, which is often useful to
work on different things or build GHC in different ways, from the same
directory/GHC checkout, without having to sacrifice the build artifacts every
time you fire up a build. This is not possible with the Make build system.

## Equivalent commands

- Building a complete stage 2 compiler with its libraries, default flavour

  ``` sh
  # Make
  make

  # Hadrian
  build
  ```

- Building with many cores

  ``` sh
  # Make
  make -j8

  # Hadrian
  build -j8
  ```

- Building a stage 1 or 2 GHC executable

  ``` sh
  # Make
  make inplace/bin/ghc-stage1
  make inplace/bin/ghc-stage2

  # Hadrian
  build stage1:exe:ghc-bin    # using the simple target name
  build _build/stage0/bin/ghc # OR using the actual path

  build stage2:exe:ghc-bin    # simple target
  build _build/stage1/bin/ghc # OR actual path
  ```

- Building and registering a library with the stage 2 compiler

  ``` sh
  # Make
  make inplace/lib/package.conf.d/text-1.2.3.0.conf

  # Hadrian
  build stage2:lib:text                                    # simple target
  build _build/stage1/lib/package.conf.d/text-1.2.3.0.conf # OR actual path
  ```

- Building with a particular flavour (e.g `quickest`)

  ``` sh
  # Make
  echo "BuildFlavour=quickest" >> mk/build.mk
  make

  # Hadrian
  build --flavour=quickest
  ```
  See [flavours documentation](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/flavours.md) for info on flavours.

- Building with `integer-simple` as the integer library

  ``` sh
  # Make
  echo "INTEGER_LIBRARY=integer-simple" >> mk/build.mk
  make

  # Hadrian
  build --integer-simple
  ```

- Freezing the stage 1 GHC compiler

  ``` sh
  # Make
  echo "stage=2" >> mk/build.mk
  make

  # Hadrian
  build --freeze1
  ```

- Running the testsuite

  ``` sh
  # Make
  make test                             # (1)
  make test TEST=plugins01              # (2)
  make test TEST="plugins01 plugins02"  # (3)
  make accept                           # (4)
  PLATFORM=YES OS=YES make accept       # (5)


  # Hadrian
  build test # equivalent to (1)

  build test --only=plugins01 # equivalent to (2)
  TEST=plugins01 build test   # equivalent to (2)

  build test --only="plugins01 plugins02"    # equivalent to (3)
  TEST="plugins01 plugins02" build test      # equivalent to (3)
  TEST=plugins01 build test --only=plugins02 # equivalent to (3)

  build test -a            # equivalent to (4)
  build test --test-accept # equivalent to (4)

  PLATFORM=YES OS=YES build test -a            # equivalent to (5)
  PLATFORM=YES OS=YES build test --test-accept # equivalent to (5)
  ```

  As illustrated in the examples above, you can use the `TEST` environment
  variable, the `--only=...` flag or even both to restrict your testsuite run
  to some (usually small) subset of the testsuite.

  See [the docs for the test rules](./testsuite.md) if you want to know about
  all the options that hadrian supports and what they correspond to in the Make
  build system.

- Generate the `platformConstants` file to be used for stage 1/2 GHC

  ``` sh
  # Make
  make inplace/lib/platformConstants

  # Hadrian
  build _build/stage0/lib/platformConstants
  build _build/stage1/lib/platformConstants
  ```

- Generate the `settings` file to be used for stage 1/2 GHC

  ``` sh
  # Make
  make inplace/lib/settings

  # Hadrian
  build _build/stage0/lib/settings
  build _build/stage1/lib/settings
  ```

- Build a static library for base with the stage 1 compiler

  ``` sh
  # Make
  make libraries/base/dist-install/build/libHSbase-4.12.0.0.a

  # Hadrian
  build _build/stage1/libraries/base/build/libHSbase-4.12.0.0.a
  ```

- Generate haddocks, user guide, etc

  ``` sh
  # Make
  make docs

  # Hadrian
  build docs
  ```

- Build documentation, but without haddocks (resp. without HTML or PDF manuals)

  ``` sh
  # Make
  echo 'HADDOCKS_DOCS = NO' > mk/build.mk
  # For HTML manuals: BUILD_SPHINX_HTML = NO
  # For PDF manuals: BUILD_SPHINX_PDF = NO
  make

  # Hadrian
  build docs --docs=no-haddocks
  # Append --docs=no-sphinx-pdfs, --docs=no-sphinx-html or
  # --docs=no-sphinx-man (or --docs=no-sphinx to encompass them all)
  # to further reduce or even completely disable documentation targets.
  ```

- Running nofib

  ``` sh
  # Make
  cd nofib; make clean; make boot; make 2>&1 | tee nofib-log

  # Hadrian
  build nofib # builds the compiler and everything we need if necessary, too
  ```
