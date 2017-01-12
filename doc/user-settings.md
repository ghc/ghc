# User settings

You can customise Hadrian by copying the file `hadrian/src/UserSettings.hs` to
`hadrian/UserSettings.hs` and overriding the default build settings (if you don't
copy the file your changes will be tracked by `git` and you can accidentally commit
them). Here we document currently supported settings.

## Build directory

Hadrian puts build results into `_build` directory by default, which is
specified by `buildRootPath`:
```haskell
-- | All build results are put into 'buildRootPath' directory.
buildRootPath :: FilePath
buildRootPath = "_build"
```

## Build flavour

Build _flavour_ is a collection of build settings that fully define a GHC build
(see `src/Flavour.hs`):
```haskell
data Flavour = Flavour
    { name               :: String    -- ^ Flavour name, to set from command line.
    , args               :: Args      -- ^ Use these command line arguments.
    , packages           :: Packages  -- ^ Build these packages.
    , integerLibrary     :: Package   -- ^ Either 'integerGmp' or 'integerSimple'.
    , libraryWays        :: Ways      -- ^ Build libraries these ways.
    , rtsWays            :: Ways      -- ^ Build RTS these ways.
    , splitObjects       :: Predicate -- ^ Build split objects.
    , buildHaddock       :: Predicate -- ^ Build Haddock and documentation.
    , dynamicGhcPrograms :: Bool      -- ^ Build dynamic GHC programs.
    , ghciWithDebugger   :: Bool      -- ^ Enable GHCi debugger.
    , ghcProfiled        :: Bool      -- ^ Build profiled GHC.
    , ghcDebugged        :: Bool }    -- ^ Build GHC with debug information.
```
Hadrian provides several
[built-in flavours](https://github.com/snowleopard/hadrian/blob/master/doc/flavours.md)
(`defaultFlavour`, `quickFlavour`, and
a few others), which can be activated from the command line, e.g. `--flavour=quick`.
Users can define new build flavours by adding them to `userFlavours` list:
```haskell
userFlavour :: Flavour
userFlavour = defaultFlavour { name = "user", ... } -- modify the default build flavour

userFlavours :: [Flavour]
userFlavours = [userFlavour]
```
Now `--flavour=user` will run Hadrian with `userFlavour` settings. In the
following sections we look at specific fields of the `Flavour` record in
more detail. Note: `defaultFlavour`, as well as its individual fields such
as `defaultArgs`, `defaultPackages`, etc. that we use below, are defined in module
`Settings.Default`. Import it as
`import {-# SOURCE #-} Settings.Default` to handle cyclic module dependencies. 

## Command line arguments

One of the key features of Hadrian is that users can easily modify any build command.
The build system will detect the change and will rerun all
affected build rules during the next build, without requiring a full rebuild.

For example, here is how to pass an extra argument `-O0` to all invocations of
GHC when compiling package `cabal`:
```haskell
userFlavour :: Flavour
userFlavour = defaultFlavour { name = "user", args = defaultArgs <> userArgs }

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

Users can add and remove packages from particular build stages. As an example,
below we add package `base` to Stage0 and remove package `haskeline` from Stage1:
```haskell
userFlavour :: Flavour
userFlavour = defaultFlavour { name = "user", packages = defaultPackages <> userPackages }

userPackages :: Packages
userPackages = mconcat
    [ stage0 ? append [base]
    , stage1 ? remove [haskeline] ]
```
If you are working on a new GHC package you need to let Hadrian know about it
by adding it to `userKnownPackages`:
```haskell
userKnownPackages :: [Package]
userKnownPackages = [userPackage]

-- An example package that lives in "libraries/user-package" directory.
userPackage :: Package
userPackage = library "user-package"
```
You will also need to add `userPackage` to a specific build stage by modifying
`userPackages` as otherwise it will not be built.

You can choose which integer library to use when builing GHC using the
`integerLibrary` setting of the build flavour. Possible values are: `integerGmp`
(default) and `integerSimple`.
```haskell
simpleFlavour :: Flavour
simpleFlavour = defaultFlavour { name = "simple", integerLibrary = integerSimple }
```
## Build ways

Packages can be built in a number of ways, such as `vanilla`, `profiling` (with
profiling information enabled), and many others as defined in `src/Way.hs`. You
can change the default build ways by modifying `libraryWays` and `rtsWays` fields
of the `Flavour` record as required. As an example, below we remove `profiling`
from the list of library ways:
```haskell
noProfilingFlavour :: Flavour
noProfilingFlavour = defaultFlavour
    { name        = "no-profiling"
    , libraryWays = defaultLibraryWays <> remove [profiling]
    , ghcProfiled = False } -- Can't build profiled GHC without profiled libraries
```
Note that `rtsWays` is computed from `libraryWays` by default, therefore the above
change will lead to the removal of `threadedProfiling` way from `rtsWays`. To
change this behaviour, you can override the default `rtsWays` setting.

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

To change the default behaviour of Hadrian with respect to building split
objects and Haddock documentation, override `splitObjects` and `buildHaddock`
fields of the `Flavour` record, for example:
```haskell
userFlavour :: Flavour
userFlavour = defaultFlavour { name = "user", splitObjects = return False, buildHaddock = return True }
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
