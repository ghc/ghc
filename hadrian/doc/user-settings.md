# Settings

You can customise Hadrian in two ways:

- By selecting a flavour, flavour transformers and using
  key-value settings, either in the command line
  or by modifying the `hadrian.settings` file in the build directory.

- By overriding the default build settings in the source code,
  by copying the file `hadrian/src/UserSettings.hs` to `hadrian/UserSettings.hs`
  and making modifications (if you don't copy the file, your changes will be tracked by `git`
  and you can accidentally commit them).

## Build flavours

A build _flavour_ is a collection of build settings that fully define a GHC build
(see `src/Flavour.hs`):
```haskell
data Flavour = Flavour {
    -- | Flavour name, to select this flavour from command line.
    name :: String,
    -- | Use these command line arguments.
    args :: Args,
    -- | Build these packages.
    packages :: Stage -> Action [Package],
    -- | Bignum backend: 'native', 'gmp', 'ffi', etc.
    bignumBackend :: String,
    -- | Check selected bignum backend against native backend
    bignumCheck :: Bool,
    -- | Build libraries these ways.
    libraryWays :: Ways,
    -- | Build RTS these ways.
    rtsWays :: Ways,
    -- | Build dynamic GHC programs.
    dynamicGhcPrograms :: Action Bool,
    -- | Build profiled GHC.
    ghcProfiled :: Stage -- ^ stage of the /built/ compiler
                -> Bool,
    -- | Build GHC with the debug RTS.
    ghcDebugged :: Stage -- ^ stage of the /built/ compiler
                -> Bool,
    -- | Build GHC with debug assertions (-DDEBUG).
    ghcDebugAssertions :: Stage -- ^ stage of the /built/ compiler
                       -> Bool,
    -- | Build the GHC executable against the threaded runtime system.
    ghcThreaded :: Stage -- ^ stage of the /built/ compiler
                -> Bool,
    -- | Whether to build docs and which ones
    --   (haddocks, user manual, haddock manual)
    ghcDocs :: Action DocTargets }
```
Hadrian provides several built-in flavours (`default`, `quick`, and a few
others; see `hadrian/doc/flavours.md`), which can be activated from the command line,
e.g. by passing `--flavour=quick`.

## Flavour transformers

Flavours can be customised using [flavour transformers](flavours.md#flavour-transformers).

For example, it is useful to enable `-Werror` when building GHC as this setting is
used in the CI to ensure a warning-free build. The `+werror` flavour transformer
can be used to easily modify a flavour to turn this setting on:

```
hadrian/build --flavour=validate+werror
```

## `key = value` and `key += value` style settings

One can supply settings from the command line or a
`<build root>/hadrian.settings` file. Hadrian currently supports
the following "families" of settings:

- `{stage0, ..., stage3, *}.(<package name> or *).ghc.{c, hs, link, deps, toolargs, *}.opts`
- `{stage0, ..., stage3, *}.(<package name> or *).cc.{c, deps, *}.opts`
- `{stage0, ..., stage3, *}.(<package name> or *).cabal.configure.opts`
- `{stage0, ..., stage3, *}.(<package name> or *).hsc2hs.run.opts`

For example, putting the following in a file at `_build/hadrian.settings`:

``` make
stage1.ghc-bin.ghc.link.opts += -debug
*.base.ghc.*.opts += -v3
```

and running hadrian with the default build root (`_build`), would respectively
link the stage 2 GHC executable (using the stage 1 GHC) with the `-debug`
flag and use `-v3` on all GHC commands used to build anything related to
`base`, whatever the stage.

We could equivalently specify those settings on the command-line:

``` sh
$ hadrian/build "stage1.ghc-bin.ghc.link.opts += -eventlog" \
                   "*.base.ghc.*.opts += -v3"
```

or specify some in `hadrian.settings` and some on the command-line.

Here is an overview of the supported settings and how you can figure out
the right names for them:

- the stage slot, which comes first, can be filled with any of `stage0`,
  `stage1`, `stage2`, `stage3` or `*`; any value but `*` will restrict the
  setting update to targets built during the given stage, while `*` is taken
  to mean "for any stage". For instance, the above example will affect
  the linking of the `_build/stage1/bin/ghc` executable.
- the package slot, which comes second, can be filled with any package name
  that Hadrian knows about (all packages that are part of a GHC checkout),
  or `*`, to respectively mean that the builder options are going to be updated
  only when building the given package, or that the said options should be used
  when building all known packages, if the Hadrian command ever gets them to be
  built;
- the remaining slots specify the builder and how it was invoked,

  * `ghc` refers to GHC commands; the final slot refers to how GHC is invoked:

    * `c.opts` for commands that build C files with GHC
    * `cpp.opts` for commands that build C++ files with GHC
	 * `hs.opts` for commands that compile Haskell modules with GHC
	  * `link.opts` for GHC linking command
	  * `deps.opts` for commands that figure out dependencies between Haskell modules
	    (with `ghc -M`)
	  * `toolargs.opts` for GHC commands that are used to generate the right ghci
	    argument for `hadrian/ghci` to work

  * `cc` refers to C/C++ compiler commands

    * `c.opts` for commands that call the C compiler on some C/C++ files
	 * `deps.opts` for commands that call the C compiler for figuring out
	    dependencies between C files. Note that this doesn't work for C++ files yet.

  * `cabal.configure.opts` refers to Cabal configure command line. Note that
    package flags can be given by adding `--flags=...` arguments. Also, for packages with `build-type: Configure`, you can pass additional arguments to the `configure` script like this: `stage1.rts.cabal.configure.opts+=--configure-option=--enable-asserts-all-ways`

  * `hsc2hs.run.opts` allows passing options to `Hsc2Hs` invocations.

  * `runtest.opts` defines extra arguments passed to `runtest.py` when
    invoked via the `hadrian test` target.

- using a wildcard (`*`) ranges over all possible values for a given "slot";
- `=` entirely overrides the arguments for a given builder in a given context,
  with the value specified on the right hand side of `=`, while `+=` merely
  extends the arguments that are to be emitted in the said context, with
  the values supplied on the right hand side of `+=`.

See `Note [Hadrian settings]` in `hadrian/src/Settings.hs` for explanations
about the implementation and how the set of supported settings can be
extended.

## Directly modifying Hadrian configuration

If the existing configuration options aren't enough for your needs,
you can directly add new configurations to Hadrian.

### Defining new flavours

Users can define new build flavours by adding them to the `userFlavours` list:
```haskell
-- | User-defined build flavours. See 'userFlavour' as an example.
userFlavours :: [Flavour]
userFlavours = [userFlavour] -- Add more build flavours if need be.

-- | This is an example user-defined build flavour. Feel free to modify it and
-- use by passing @--flavour=user@ from the command line.
userFlavour :: Flavour
userFlavour = defaultFlavour { name = "user" } -- Modify other settings here.
```
Now `--flavour=user` will run Hadrian with `userFlavour` settings.

When no `--flavour` argument is passed to hadrian, it will use the
`default` one. You can however change this, and for example make
the "fallback" flavour be `user`, by changing `userDefaultFlavour`:

``` haskell
userDefaultFlavour :: String
-- before:
-- userDefaultFlavour = "default"
-- now:
userDefaultFlavour = "user"
```

This saves you from having to type `build --flavour=user [...]`
every time, allowing you to _persist_ the choice of flavour.

In the
following sections we look at specific fields of the `Flavour` record in
more detail. Note: `defaultFlavour`, as well as its individual fields such
as `defaultArgs`, `defaultPackages`, etc. that we use below, are defined in module
`Settings.Default`.

### Passing command line arguments to builders

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
are currently built as part of the GHC are defined in `src/Packages.hs`.

You can combine several custom command line settings using `mconcat`:
```haskell
userArgs :: Args
userArgs = mconcat
    [ builder Ghc ? package cabal ? arg "-O0"
    , package rts ? input "**/PrimOps.c" ? pure ["-fno-PIC", "-static"] ]
```
You can match any combination of the `builder`, `stage`, `package`, `way`, `input`
and `output` predicates when specifying custom command line arguments. File
patterns such as `"**/Prelude.*"` can be used when matching input and output files,
where `**` matches an arbitrary number of path components, but not absolute path
prefixes, and `*` matches an entire path component, excluding any separators.

#### Linking GHC against the debugged RTS

What was previously achieved by having `GhcDebugged=YES` in `mk/build.mk` can
be done by defining a custom flavour in the user settings file, one that
sets the `ghcDebugged` field of `Flavour` to `const True`, e.g:

``` haskell
quickDebug :: Flavour
quickDebug = quickFlavour { name = "dbg", ghcDebugged = const True }
```

Running `build --flavour=dbg` will build a `quick`-flavoured GHC and link
GHC, iserv, iserv-proxy and remote-iserv against the debugged RTS, by passing
`-debug` to the commands that link those executables.

More generally, a predicate on `Stage` can be provided to specify which stages should be built debugged. For example, setting `ghcDebugged = (>= Stage2)` will build a debugged compiler at stage 2 or higher, but not stage 1.

Finally, the `debug_ghc` and `debug_stage1_ghc` [flavour transformers](#flavour-transformers) provide a convenient way to enable `ghcDebugged` on the command line without the need to define a separate custom flavour.

### Packages

Users can add and remove packages from particular build stages. As an example,
below we add package `base` to Stage0 and remove package `haskeline` from Stage1:
```haskell
...
import Packages
...

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

You can choose which Bignum backend to use when buidling GHC using the
`bignumBackend` setting of the build flavour. Possible values are: `gmp`
(default), `native` or `ffi`.
```haskell
userFlavour :: Flavour
userFlavour = defaultFlavour { name = "user", bignumBackend = "native" }
```

#### Specifying the final stage to build

The `finalStage` variable can be set to indicate after which stage we should
stop the compilation pipeline. By default it is set to `Stage2` which indicates
that we will build everything which uses the `Stage1` `ghc` and then stop.

```
finalStage :: Stage
finalStage = Stage2
```

Using this mechanism we can also build a `Stage3` compiler by setting
`finalStage = Stage3` or just a `Stage1` compiler by setting
`finalStage = Stage1`.

### Build ways

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

Similarly, if we want to completely turn off dynamic linking, we can define a custom
`Flavour` to this effect:
``` haskell
noDynamicFlavour :: Flavour
noDynamicFlavour = defaultFlavour
    { name = "no-dynamic"
    , libraryWays = remove [dynamic] defaultLibraryWays }
```

### Documentation

`Flavour`'s `ghcDocs :: Action DocTargets` field lets you
customize the "groups" of documentation targets that should
run when running `build docs` (or, transitively,
`build binary-dist`).

```haskell
type DocTargets = Set DocTarget
data DocTarget = Haddocks | SphinxHTML | SphinxPDFs | SphinxMan
```

By default, `ghcDocs` contains all of them and `build docs` would
therefore attempt to build all the haddocks, manuals and manpages.
If, for some reason (e.g no easy way to install `sphinx-build` or
`xelatex` on your system), you're just interested in building the
haddocks, you could define a custom flavour as follows:

```haskell
justHaddocksFlavour :: Flavour
justHaddocksFlavour = defaultFlavour
    { name = "default-haddocks"
	, ghcDocs = Set.singleton Haddocks }
```

and then run `build --flavour=default-haddocks`. Alternatively,
you can use the `--docs` CLI flag to selectively disable some or
all of the documentation targets:

- `--docs=none`: don't build any docs
- `--docs=no-haddocks`: don't build haddocks
- `--docs=no-sphinx`: don't build any user manual or manpage
- `--docs=no-sphinx-html`: don't build HTML versions of manuals
- `--docs=no-sphinx-pdfs`: don't build PDF versions of manuals
- `--docs=no-sphinx-man`: don't build the manpage

You can pass several `--docs=...` flags, Hadrian will combine
their effects.

### Split sections

You can build all or just a few packages with
[`-split-sections`][split-sections] by tweaking an existing
flavour (whichever matches your needs) using
`splitSections` or `splitSectionsIf`:

``` haskell
splitSections :: Flavour -> Flavour
splitSectionsIf :: (Package -> Bool) -> Flavour -> Flavour
```

For example, you can easily start with the `quick` flavour and
additionally build all Haskell packages with `-split-sections` by defining a new
flavour as
`(splitSectionsIf (const True) quickFlavour) { name = "quick-split" }`.
You can then start a build with this flavour with `build --flavour=quick-split`.

Changing `(const True)` to `(== base)` would only build `base` with
`-split-sections`, not all Haskell packages as with `quick-split` above.

`splitSections` is simply `splitSectionsIf` applied to the predicate
`(/=ghc)`, i.e it builds all Haskell packages but the `ghc`
library with `-split-sections` (it is usually not worth using that
option with the `ghc` library).

### Miscellaneous

Hadrian prints various progress info during the build. You can change the colours
used by default by overriding `buildProgressColour` and `successColour`:
```haskell
-- | Set colour for build progress messages (e.g. executing a build command).
buildProgressColour :: BuildProgressColour
buildProgressColour = mkBuildProgressColour (Dull Magenta)

-- | Set colour for success messages (e.g. a package is built successfully).
successColour :: SuccessColour
successColour = mkSuccessColour (Dull Green)
```

Your options are `Dull Colour`, `Vivid Colour`, or `Extended Code`. `Dull`
colours are the ANSI 8-bit colours, `Vivid` correspond to the 16-bit codes that
end with ";1", and `Extended` let's you enter a manual code for the 256 colour
set. E.g.

```
Dull Blue
Vivid Cyan
Extended "203"
```

### Tab completion

Hadrian supports tab-completion for the key-value settings. This is implemented
in `Rules.SimpleTargets.completionRule`, by exporting an `autocomplete` target
that takes an (optional) argument, `--complete-setting=<some string>`, and
prints on stdout all the setting keys that have the given string as a prefix.

There is a `hadrian/completion.sh` script that makes use of this rule to
install Bash completions for `hadrian/build` and `hadrian/build-cabal`.
You can try it out by doing:

``` sh
$ source hadrian/completion.sh
$ hadrian/build <TAB>
$ hadrian/build stage1.ba<TAB>
$ hadrian/build "stage1.base.ghc.<TAB>
$ hadrian/build "*.*.ghc.*.opts += -v3" "stage0.ghc-bin.ghc.lin<TAB>
```

[split-sections]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#ghc-flag--split-sections
