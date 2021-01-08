# Building GHC

* Add the following to _build/hadrian.settings

```
stage1.*.ghc.hs.opts += -finfo-table-map -fdistinct-constructor-tables
```

* Build GHC as normal

```
./hadrian/build -j8
```

* The result is a ghc-debug enabled compiler

# Building a debugger

* Use the compiler you just built to build ghc-debug

```
cd ghc-debug
cabal update
cabal new-build debugger -w ../_build/stage1/bin/ghc
```

# Running the debugger

Modify `test/Test.hs` to implement the debugging thing you want to do. Perhaps
start with `p30`, which is a program to generate a profile.


* Start the process you want to debug
```
GHC_DEBUG_SOCKET=/tmp/ghc-debug build-cabal
```

* Start the debugger
```
cabal new-run debugger -w ...
```

* Open a ticket about the memory issue you find.


