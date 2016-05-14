# User settings

Users can customise Hadrian by specifying user build settings in file
`src/Settings/User.hs`. Here we document currently supported settings.

## Build directory

Hadrian puts build results into `_build` directory by default, which is
controlled by `buildRootPath`:
```haskell
-- | All build artefacts are stored in 'buildRootPath' directory.
buildRootPath :: FilePath
buildRootPath = "_build"
```

## Command line arguments

One of the key features of Hadrian is that users can modify any build command by
changing `userArgs`. The build system will detect the change and will rerun all
affected build rules during the next build, without requiring a full rebuild.

As an example, here is how to pass an extra argument `-O0` to all invocations of
GHC when compiling package `cabal`:
```haskell
-- | Control user-specific command line arguments.
userArgs :: Args
userArgs = builder Ghc ? package cabal ? arg "-O0"
```
Builders such as `Ghc` are defined in `src/Builder.hs`, and all packages that
are currently built as part of the GHC are defined in `src/GHC.hs` (also see
`src/Package.hs`).

It is possible to specify several custom command line arguments combining the
list with `mconcat`:
```haskell
userArgs :: Args
userArgs = mconcat 
    [ builder Ghc ? package cabal ? arg "-O0"
    , package rts ? input "//Evac\_thr.c" ? append [ "-DPARALLEL\_GC", "-Irts/sm" ]
    , builder Ghc ? output "//Prelude.\*" ? remove ["-Wall", "-fwarn-tabs"] ]
```
The above example also demostrates the use of `append` for adding more than one
argument and `remove` for removing arguments that Hadrian uses by default. It is
possible to match any combination of the current `builder`, `stage`, `package`,
`way`, `input` and `output` using predicates. File patterns such as
`"//Prelude.\*"` can be used when matching input and output files where `//`
matches an arbitrary number of path components and `\*` matches an entire path component, excluding any separators.

## Packages

To add or remove a package from a particular build stage, use `userPackages`. As
an example, below we add package `base` to Stage0 and remove package `haskeline`
from Stage1:
```haskell
-- | Control which packages get to be built.
userPackages :: Packages
userPackages = mconcat
    [ stage0 ? append [base]
    , stage1 ? remove [haskeline] ]
```
If you are working on a new GHC package you need to let Hadrian know about it
by setting `userKnownPackages`:
```haskell
-- | Add new user-defined packages.
userKnownPackages :: [Package]
userKnownPackages = []
```
To control which integer library to use when builing GHC, set `integerLibrary`:
```haskell
-- | Choose the integer library: integerGmp or integerSimple.
integerLibrary :: Package
integerLibrary = integerGmp
```

## Build ways

Libraries can be built in a number of ways, such as `vanilla`, `profiling` (with 
profiling information enabled), and many others as defined in `src/Way.hs`. To
control which ways particular packages are built, set `userLibraryWays` and
`userRtsWays`. As an example, below we remove `dynamic` from the list of library
ways and keep `rts` package ways unchanged:
```haskell
-- | Control which ways library packages are built.
userLibraryWays :: Ways
userLibraryWays = remove [dynamic]

-- | Control which ways the 'rts' package is built.
userRtsWays :: Ways
userRtsWays = mempty
```

## Verbose command lines 

By default Hadrian does not print full command lines during the build process
and instead prints short human readable digests for each executed command. It is
possible to suppress this behaviour completely or partially using
`verboseCommands` setting:
```haskell
-- | Set to True to print full command lines during the build process. Note,
-- this is a Predicate, hence you can enable verbose output for a chosen package
-- only, e.g.: verboseCommands = package ghcPrim
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
verboseCommands = file "//rts/sm/*" &&^ way threaded

-- Show all commands:
verboseCommands = return True
```