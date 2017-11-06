# User settings

You can customise Hadrian by copying the file `hadrian/src/UserSettings.hs` to
`hadrian/UserSettings.hs` and overriding the default build settings (if you don't
copy the file your changes will be tracked by `git` and you can accidentally commit
them). Here we document currently supported settings.

## Build directory

Hadrian puts build results into `_build` directory by default, which is
specified by `userBuildRoot`:
```haskell
-- | All build results are put into the 'buildRoot' directory.
userBuildRoot :: BuildRoot
userBuildRoot = BuildRoot "_build"
```

## Build flavour

Build _flavour_ is a collection of build settings that fully define a GHC build
(see `src/Flavour.hs`):
```haskell
data Flavour = Flavour {
    -- | Flavour name, to select this flavour from command line.
    name :: String,
    -- | Use these command line arguments.
    args :: Args,
    -- | Build these packages.
    packages :: Stage -> Action [Package],
    -- | Either 'integerGmp' or 'integerSimple'.
    integerLibrary :: Action Package,
    -- | Build libraries these ways.
    libraryWays :: Ways,
    -- | Build RTS these ways.
    rtsWays :: Ways,
    -- | Build split objects.
    splitObjects :: Predicate,
    -- | Build dynamic GHC programs.
    dynamicGhcPrograms :: Bool,
    -- | Enable GHCi debugger.
    ghciWithDebugger :: Bool,
    -- | Build profiled GHC.
    ghcProfiled :: Bool,
    -- | Build GHC with debug information.
    ghcDebugged :: Bool }
```
Hadrian provides several built-in flavours (`defaultFlavour`, `quickFlavour`, and a few
others; see `hadrian/doc/flavours.md`), which can be activated from the command line,
e.g. by passing `--flavour=quick`. Users can define new build flavours by adding them
to `userFlavours` list:
```haskell
-- | User-defined build flavours. See 'userFlavour' as an example.
userFlavours :: [Flavour]
userFlavours = [userFlavour] -- Add more build flavours if need be.

-- | This is an example user-defined build flavour. Feel free to modify it and
-- use by passing @--flavour=user@ from the command line.
userFlavour :: Flavour
userFlavour = defaultFlavour { name = "user" } -- Modify other settings here.
```
Now `--flavour=user` will run Hadrian with `userFlavour` settings. In the
following sections we look at specific fields of the `Flavour` record in
more detail. Note: `defaultFlavour`, as well as its individual fields such
as `defaultArgs`, `defaultPackages`, etc. that we use below, are defined in module
`Settings.Default`.

## Command line arguments

One of the key features of Hadrian is that users can easily modify any build command.
The build system will detect the change and will rerun all affected build rules during
the next build, without requiring a full rebuild.

For example, here is how to pass an extra argument `-O0` to all invocations of
GHC when compiling package `cabal`:
```haskell
userFlavour :: Flavour
userFlavour = defaultFlavour { name = "user", args = defaultArgs <> userArgs }

userArgs :: Args
userArgs = builder Ghc ? package cabal ? arg "-O0"
```
Builders such as `Ghc` are defined in `src/Builder.hs`, and all packages that
are currently built as part of the GHC are defined in `src/GHC.hs`.

You can combine several custom command line settings using `mconcat`:
```haskell
userArgs :: Args
userArgs = mconcat
    [ builder Ghc ? package cabal ? arg "-O0"
    , package rts ? input "//Evac_thr.c" ? pure [ "-DPARALLEL_GC", "-Irts/sm" ] ]
```
You can match any combination of the `builder`, `stage`, `package`, `way`, `input`
and `output` predicates when specifying custom command line arguments. File
patterns such as `"//Prelude.*"` can be used when matching input and output files,
where `//` matches an arbitrary number of path components and `*` matches an entire
path component, excluding any separators.

## Packages

Users can add and remove packages from particular build stages. As an example,
below we add package `base` to Stage0 and remove package `haskeline` from Stage1:
```haskell
userFlavour :: Flavour
userFlavour = defaultFlavour { name = "user", packages = modifiedPackages }

modifiedPackages :: Stage -> Action [Package]
modifiedPackages stage = do
    packages <- defaultPackages stage
    return $ case stage of
        Stage0 -> packages ++ [base]
        Stage1 -> packages \\ [haskeline]
        _      -> packages
```
If you are working on a new GHC package you need to let Hadrian know about it
by adding it to `userPackages`:
```haskell
userPackages :: [Package]
userPackages = [userPackage]

-- An example package that lives in "libraries/user-package" directory.
userPackage :: Package
userPackage = library "user-package"
```
You will also need to add `userPackage` to a specific build stage by modifying
the `packages` setting of the user flavour as otherwise it will not be built.

You can choose which integer library to use when builing GHC using the
`integerLibrary` setting of the build flavour. Possible values are: `integerGmp`
(default) and `integerSimple`.
```haskell
userFlavour :: Flavour
userFlavour = defaultFlavour { name = "user", integerLibrary = integerSimple }
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
    , libraryWays = remove [profiling] defaultLibraryWays
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
-- | Set to 'True' to print full command lines during the build process. Note:
-- this is a 'Predicate', hence you can enable verbose output only for certain
-- targets, e.g.: @verboseCommand = package ghcPrim@.
verboseCommand :: Predicate
verboseCommand = do
    verbosity <- expr getVerbosity
    return $ verbosity >= Loud
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

By setting `stage1Only = True` you can disable building Stage2 GHC (i.e. the
`ghc-stage2` executable) and Stage2 utilities, such as `haddock`. Note that all
Stage0 and Stage1 libraries (including `compiler`) will still be built. Enabling
this flag during installation leads to installing `ghc-stage1` instead of
`ghc-stage2`, and `ghc-pkg` that was build with the Stage0 compiler.

To change the default behaviour of Hadrian with respect to building split
objects, override the `splitObjects` setting of the `Flavour` record:
```haskell
userFlavour :: Flavour
userFlavour = defaultFlavour { name = "user", splitObjects = return False }
```

Hadrian prints various progress info during the build. You can change the colours
used by default by overriding `buildProgressColour` and `successColour`:
```haskell
-- | Set colour for build progress messages (e.g. executing a build command).
buildProgressColour :: BuildProgressColour
buildProgressColour = BuildProgressColour (Dull, Magenta)

-- | Set colour for success messages (e.g. a package is built successfully).
successColour :: SuccessColour
successColour = SuccessColour (Dull, Green)
```
