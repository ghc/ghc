# User settings

You can customise Hadrian by copying the file ./src/UserSettings.hs to
./UserSettings.hs and specifying user build settings in
`./UserSettings.hs`. Here we document currently supported settings.

## Build directory

Hadrian puts build results into `_build` directory by default, which is
specified by `buildRootPath`:
```haskell
-- | All build results are put into 'buildRootPath' directory.
buildRootPath :: FilePath
buildRootPath = "_build"
```

## Command line arguments

One of the key features of Hadrian is that users can modify any build command by
changing `userArgs`. The build system will detect the change and will rerun all
affected build rules during the next build, without requiring a full rebuild.

For example, here is how to pass an extra argument `-O0` to all invocations of
GHC when compiling package `cabal`:
```haskell
-- | Modify default build command line arguments.
userArgs :: Args
userArgs = builder Ghc ? package cabal ? arg "-O0"
```
Builders such as `Ghc` are defined in `src/Builder.hs`, and all packages that
are currently built as part of the GHC are defined in `src/GHC.hs`. See also
`src/Package.hs`.

You can combine several custom command line settings using `mconcat`:
```haskell
userArgs :: Args
userArgs = mconcat
    [ builder Ghc ? package cabal ? arg "-O0"
    , package rts ? input "//Evac_thr.c" ? append [ "-DPARALLEL_GC", "-Irts/sm" ]
    , builder Ghc ? output "//Prelude.*" ? remove ["-Wall", "-fwarn-tabs"] ]
```
The above example also demostrates the use of `append` for adding more than one
argument and `remove` for removing arguments that Hadrian uses by default. You
can match any combination of the `builder`, `stage`, `package`, `way`, `input`
and `output` predicates when specifying custom command line arguments. File
patterns such as `"//Prelude.*"` can be used when matching input and output files,
where `//` matches an arbitrary number of path components and `*` matches an entire
path component, excluding any separators.

## Packages

To add or remove a package from a particular build stage, use `userPackages`. As
an example, below we add package `base` to Stage0 and remove package `haskeline`
from Stage1:
```haskell
-- | Modify the set of packages that are built by default in each stage.
userPackages :: Packages
userPackages = mconcat
    [ stage0 ? append [base]
    , stage1 ? remove [haskeline] ]
```
If you are working on a new GHC package you need to let Hadrian know about it
by setting `userKnownPackages`:
```haskell
-- | Add user defined packages. Don't forget to add them to 'userPackages' too.
userKnownPackages :: [Package]
userKnownPackages = [myPackage]

-- An example package that lives in "libraries/my-package" directory.
myPackage :: Package
myPackage = library "my-package"
```
Note, you will also need to add `myPackage` to a specific build stage by modifying
`userPackages` as otherwise it will not be built.

You can choose which integer library to use when builing GHC by setting
`integerLibrary`. Possible values are: `integerGmp` (default) and `integerSimple`.
```haskell
-- | Choose the integer library: 'integerGmp' or 'integerSimple'.
integerLibrary :: Package
integerLibrary = integerGmp
```
## Build ways

Packages can be built in a number of ways, such as `vanilla`, `profiling` (with
profiling information enabled), and many others as defined in `src/Way.hs`. You
can change the default build ways using `userLibraryWays` and `userRtsWays` settings.
As an example, below we remove `dynamic` from the list of library ways but keep
`rts` package ways unchanged:
```haskell
-- | Modify the set of ways in which library packages are built.
userLibraryWays :: Ways
userLibraryWays = remove [dynamic]

-- | Modify the set of ways in which the 'rts' package is built.
userRtsWays :: Ways
userRtsWays = mempty
```

## Verbose command lines

By default Hadrian does not print full command lines during the build process
and instead prints short human readable digests for each executed command. You
can suppress this behaviour completely or partially using `verboseCommands` setting:
```haskell
-- | Set to True to print full command lines during the build process. Note,
-- this is a Predicate, hence you can enable verbose output only for certain
-- targets, e.g.: @verboseCommands = package ghcPrim@.
verboseCommands :: Predicate
verboseCommands = return False
```
For example, to print the full command lines used to compile GHC executables,
set `verboseCommands` to:
```haskell
verboseCommands :: Predicate
verboseCommands = input "ghc/Main.hs"
```
Below are a few other examples:
```haskell
-- Print command lines for all Ghc Link invocations:
verboseCommands = builder (Ghc Link)

-- Print command lines when compiling files in package compiler using Gcc:
verboseCommands = builder (Gcc Compile) &&^ package compiler

-- Use patterns when matching files:
verboseCommands = output "//rts/sm/*" &&^ way threaded

-- Print all commands:
verboseCommands = return True
```

## Miscellaneous

Use the following settings to change the default behaviour of Hadrian with respect
to building split objects and Haddock documentation.

```haskell
-- | Control when split objects are generated. Note, due to the GHC bug #11315
-- it is necessary to do a full clean rebuild when changing this option.
splitObjects :: Predicate
splitObjects = (return cmdSplitObjects) &&^ defaultSplitObjects

-- | Control when to build Haddock documentation.
buildHaddock :: Predicate
buildHaddock = return cmdBuildHaddock
```

Hadrian prints various progress info during the build. You can customise how this
info is printed by overriding `putBuild` and `putSuccess` commands:

```haskell
-- | Customise build progress messages (e.g. executing a build command).
putBuild :: String -> Action ()
putBuild = putColoured Vivid White

-- | Customise build success messages (e.g. a package is built successfully).
putSuccess :: String -> Action ()
putSuccess = putColoured Vivid Green
```

You can tune colours for your favourite terminal and also change the verbosity
level, e.g. by setting `putSuccess = putLoud`, which will hide success messages
unless Hadrian is called with `--verbose` flag.
