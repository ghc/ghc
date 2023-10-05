## General tips on debugging Haskell

https://wiki.haskell.org/Debugging

## General tips on debugging shake

http://shakebuild.com/debugging

## Make-based old system

1. `make show! VALUE=$(NAME)$` to check the value of variable `NAME`

## Debugging Hadrian

### General thoughts

1. If a build command with some flags doesn't work, will that work on the Make-based old system? (Note that the directory structure is different, so we need to do some conversion)
2. If you delete something, but run top-level target again doesn't produce that, then it must be that this things is not correctly registered as tracked output

### Verbosity

Adding `-V`, `-VV`, `-VVV` can output more information from Shake and Hadrian for debugging purpose. For example, to print out the complete commands, you can use `-V`.

### Common pitfalls

- Out-of-date `UserSetting.hs`? (may cause compilation errors while building hadrian itself)
- Path: absolute? relative? platform-dependent?
- Missing environment variables?
- The upstream GHC/Cabal/... has updated

### How to read exception message

#### Type 1: `openFile: does not exist`

**Example:** `hadrian: _build/stage1/ghc/GHCi/UI.o: openFile: does not exist (No such file or directory)`

**Cause:** The build system tries to execute some *raw* I/O operation, which is not captured properly by shake build or any other handling mechanism.

**Solution:** Use shake-provided or `Util`-provided I/O actions which can automatically make input files *dependencies*, and throw better exception (like below) when no rule for building these files exists.

#### Type 2: `Error when running Shake build system:`

Example:

> TODO
>
> Make a new example because `ghc-cabal` no longer exists.
> New developers will not know what it is and thus find the example below more confusing.

```
Error when running Shake build system:
* OracleQ (PackageDataKey ("_build/stage1/libraries/unix/package-data.mk","COMPONENT_ID"))
* _build/stage1/libraries/unix/package-data.mk
* OracleQ (PackageDataKey ("_build/stage1/libraries/base/package-data.mk","COMPONENT_ID"))
* _build/stage1/libraries/base/package-data.mk
* OracleQ (PackageDataKey ("_build/stage1/libraries/ghc-prim/package-data.mk","COMPONENT_ID"))
* _build/stage1/libraries/ghc-prim/package-data.mk
* _build/stage1/libraries/ghc-prim/GHC/Prim.hs
* inplace/bin/genprimopcode
* OracleQ (PackageDataKey ("_build/stage0/utils/genprimopcode/package-data.mk","C_SRCS"))
* _build/stage0/utils/genprimopcode/package-data.mk
user error (Development.Shake.cmd, system command failed
Command: _build/stage0/utils/ghc-cabal/ghc-cabal configure utils/genprimopcode /Users/zz/Repos/ghc2/_build/stage0/utils/genprimopcode '' --with-ghc=/usr/local/bin/ghc --with-ghc-pkg=/usr/local/bin/ghc-pkg --package-db=_build/stage0/bootstrapping.conf --enable-library-vanilla --enable-library-for-ghci --disable-library-profiling --disable-shared --with-hscolour=/Users/zz/Library/Haskell/bin/HsColour '--configure-option=CFLAGS=-fno-stack-protector -I/Users/zz/Repos/ghc2/_build/generated' --configure-option=CPPFLAGS=-I_build/generated '--gcc-options=-fno-stack-protector -I/Users/zz/Repos/ghc2/_build/generated' --configure-option=--with-cc=/usr/bin/gcc --constraint 'Cabal == 2.0.0.0' --constraint 'binary == 0.8.4.1' --constraint 'ghc-boot == 8.3' --constraint 'ghc-boot-th == 8.3' --constraint 'ghci == 8.3' --constraint 'hoopl == 3.10.2.2' --constraint 'hpc == 0.6.0.3' --constraint 'template-haskell == 2.12.0.0' --constraint 'terminfo == 0.4.1.0' --constraint 'transformers == 0.5.2.0' --with-gcc=/usr/bin/gcc --with-ar=/usr/bin/ar --with-alex=/Users/zz/Library/Haskell/bin/alex --with-happy=/Users/zz/Library/Haskell/bin/happy -v0 --configure-option=--quiet --configure-option=--disable-option-checking
Exit code: 1
Stderr:
ghc-cabal: ghc-pkg dump failed: dieVerbatim: user error (ghc-cabal:
'/usr/local/bin/ghc-pkg' exited with an error:
ghc-pkg: _build/stage0/bootstrapping.conf: getDirectoryContents: does not
exist (No such file or directory)
)
)
```

First, the list of `OracleQ`s is similar to a call trace. Each `OracleQ` line specifies the *target*, and lines without `OracleQ` after it specify dependencies. Each dependency is the *target* of the next level.

The last level's target in the above example is `_build/stage0/utils/genprimopcode/package-data.mk`. Build system tries to build it with the command as shown in `Command` line (this is very useful -- since you can copy & paste and tweak with it directly when trying find the right rule).

The error dumped after the `Command` line can help you identify the potential bug quickly.

### Run some snippets

Sometimes it is useful to run a few lines of code for debugging purpose, for example, print out the dependencies of `cabal`. The fastest way to do this is to modify the `Main.hs`, comment out the heavy-lifting rules, add insert the code you'd like to run.

```haskell
        Rules.Clean.cleanRules
        Rules.Oracles.oracleRules
        -- Rules.SourceDist.sourceDistRules
        -- Rules.Selftest.selftestRules
        -- Rules.Test.testRules
        -- Rules.buildRules
        -- Rules.topLevelTargets
        rulesToTest
```

## How to enable stack trace

first, build shake with profile support:

```
# inside shake source directory
cabal install --enable-profiling
```

```
# for an example source file
cabal exec -- ghc -prof -fprof-auto examples/Main.hs
```

and a work-in-progress [traced fork of shake](https://github.com/monad-systems/shake/tree/traced)
