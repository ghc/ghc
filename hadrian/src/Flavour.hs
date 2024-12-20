module Flavour
  ( Flavour (..), werror
  , DocTargets, DocTarget(..)
  , parseFlavour
    -- * Flavour transformers
  , flavourTransformers
  , addArgs
  , splitSections
  , enableThreadSanitizer
  , enableLateCCS
  , enableDebugInfo, enableTickyGhc
  , viaLlvmBackend
  , enableProfiledGhc
  , disableDynamicGhcPrograms
  , disableDynamicLibs
  , disableProfiledLibs
  , enableLinting
  , enableHaddock
  , enableHiCore
  , useNativeBignum
  , enableTextWithSIMDUTF
  , omitPragmas

  , completeSetting
  , applySettings
  ) where

import Expression
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as Set
import Packages
import Flavour.Type
import Settings.Parser

import Text.Parsec.Prim as P
import Text.Parsec.Combinator as P
import Text.Parsec.Char as P
import Control.Monad.Except
import UserSettings


flavourTransformers :: Map String (Flavour -> Flavour)
flavourTransformers = M.fromList
    [ "werror"           =: werror
    , "debug_info"       =: enableDebugInfo
    , "ticky_ghc"        =: enableTickyGhc
    , "split_sections"   =: splitSections
    , "no_split_sections" =: noSplitSections
    , "thread_sanitizer" =: enableThreadSanitizer False
    , "thread_sanitizer_cmm" =: enableThreadSanitizer True
    , "llvm"             =: viaLlvmBackend
    , "profiled_ghc"     =: enableProfiledGhc
    , "no_dynamic_ghc"   =: disableDynamicGhcPrograms
    , "no_dynamic_libs"  =: disableDynamicLibs
    , "native_bignum"    =: useNativeBignum
    , "text_simdutf"     =: enableTextWithSIMDUTF
    , "no_profiled_libs" =: disableProfiledLibs
    , "omit_pragmas"     =: omitPragmas
    , "ipe"              =: enableIPE
    , "fully_static"     =: fullyStatic
    , "host_fully_static" =: hostFullyStatic
    , "collect_timings"  =: collectTimings
    , "assertions"       =: enableAssertions
    , "debug_ghc"        =: debugGhc Stage2
    , "debug_stage1_ghc" =: debugGhc Stage1
    , "lint"             =: enableLinting
    , "haddock"          =: enableHaddock
    , "hi_core"          =: enableHiCore
    , "late_ccs"         =: enableLateCCS
    , "boot_nonmoving_gc" =: enableBootNonmovingGc
    , "dump_stg"         =: enableDumpStg
    ]
  where (=:) = (,)

type Parser = Parsec String ()

parseFlavour :: [Flavour]  -- ^ base flavours
             -> Map String (Flavour -> Flavour) -- ^ modifiers
             -> String
             -> Either String Flavour
parseFlavour baseFlavours transformers str =
    case P.runParser parser () "" str of
      Left perr -> Left $ unlines $
                   [ "error parsing flavour specifier: " ++ show perr
                    , ""
                    , "known flavours:"
                    ] ++
                    [ "  " ++ name f | f <- baseFlavours ] ++
                    [ ""
                    , "known flavour transformers:"
                    ] ++
                    [ "  " ++ nm | nm <- M.keys transformers ]
      Right f -> Right f
  where
    parser :: Parser Flavour
    parser = do
      base <- baseFlavour
      transs <- P.many flavourTrans
      P.eof
      return $ foldr ($) base transs

    baseFlavour :: Parser Flavour
    baseFlavour =
        P.choice [ f <$ P.try (P.string (name f))
                 | f <- reverse (sortOn name baseFlavours)
                 ]      -- reverse&sort needed to parse e.g. "quick-debug" before "quick"

    flavourTrans :: Parser (Flavour -> Flavour)
    flavourTrans = do
        void $ P.char '+'
        P.choice [ trans <$ P.try (P.string nm)
                 | (nm, trans) <- reverse $ sortOn fst $ M.toList transformers
                      -- reverse&sort needed to parse e.g. "ticky_ghc0" before "ticky_ghc"
                 ]

-- | Add arguments to the 'args' of a 'Flavour'.
addArgs :: Args -> Flavour -> Flavour
addArgs args' fl = fl { extraArgs = extraArgs fl <> args' }

-- | Turn on -Werror for packages built with the stage1 compiler.
-- It mimics the CI settings so is useful to turn on when developing.

-- TODO: the -Wwarn flags are added to make validation flavour works
-- for cross-compiling unix-2.8.0.0. There needs to be further fixes
-- in unix and/or hsc2hs to make cross-compiling unix completely free
-- from warnings.
werror :: Flavour -> Flavour
werror =
  addArgs $ mconcat
    [ builder Ghc
        ? notStage0
        ? mconcat
          [ arg "-Werror"
            -- unix has many unused imports
          , package unix
              ? mconcat [arg "-Wwarn=unused-imports", arg "-Wwarn=unused-top-binds"]
            -- semaphore-compat relies on sem_getvalue as provided by unix, which is
            -- not implemented on Darwin and therefore throws a deprecation warning
          , package semaphoreCompat
              ? mconcat [arg "-Wwarn=deprecations"]
          ]
    , builder Ghc
        ? package rts
        ? mconcat
          [ arg "-optc-Werror"
            -- clang complains about #pragma GCC pragmas
          , arg "-optc-Wno-error=unknown-pragmas"
            -- rejected inlinings are highly dependent upon toolchain and way
          , arg "-optc-Wno-error=inline"
          ]
      -- N.B. We currently don't build the boot libraries' C sources with -Werror
      -- as this tends to be a portability nightmare.
    ]

-- | Build C and Haskell objects with debugging information.
enableDebugInfo :: Flavour -> Flavour
enableDebugInfo = addArgs $ notStage0 ? mconcat
    [ builder (Ghc CompileHs) ? pure ["-g3"]
    , builder (Ghc CompileCWithGhc) ? pure ["-optc-g3"]
    , builder (Cc CompileC) ? arg "-g3"
    , builder (Cabal Setup) ? arg "--disable-library-stripping"
    , builder (Cabal Setup) ? arg "--disable-executable-stripping"
    ]

-- | Enable the ticky-ticky profiler in stage2 GHC
enableTickyGhc :: Flavour -> Flavour
enableTickyGhc f =
    (addArgs (orM [stage1, cross] ? mconcat
      [ builder (Ghc CompileHs) ? tickyArgs
      , builder (Ghc LinkHs) ? tickyArgs
      ]) f) { ghcThreaded = (< Stage2) }
      -- Build single-threaded ghc because ticky profiling is racy with threaded
      -- RTS and the C counters are disabled. (See #23439)

tickyArgs :: Args
tickyArgs = mconcat
  [ arg "-ticky"
  , arg "-ticky-allocd"
  , arg "-ticky-dyn-thunk"
  ]

enableDumpStg :: Flavour -> Flavour
enableDumpStg =
  addArgs $ stage1 ?
    builder (Ghc CompileHs) ? mconcat [ arg "-ddump-to-file", arg "-ddump-stg-final" ]

-- | Enable Core, STG, and (not C--) linting in all compilations with the stage1 compiler.
enableLinting :: Flavour -> Flavour
enableLinting =
    addArgs $ stage1 ? mconcat
      [ builder (Ghc CompileHs) ? lint
      ]
  where
    lint = mconcat
      [ arg "-dlint"
      ]

-- | Enable Haddock documentation.
enableHaddock :: Flavour -> Flavour
enableHaddock =
    addArgs $ stage1 ? mconcat
      [ builder (Ghc CompileHs) ? haddock
      ]
  where
    haddock = mconcat
      [ arg "-haddock"
      ]

-- | Build stage2 dependencies with options to emit Core into
-- interface files which is sufficient to restart code generation.
enableHiCore :: Flavour -> Flavour
enableHiCore = addArgs
    $ notStage0 ? builder (Ghc CompileHs)
    ? pure ["-fwrite-if-simplified-core"]

-- | Transform the input 'Flavour' so as to build with
--   @-split-sections@ whenever appropriate.
--   Note that this transformer doesn't do anything
--   on darwin because on darwin platforms we always enable subsections
--   via symbols.
splitSections, noSplitSections :: Flavour -> Flavour
splitSections f = f { ghcSplitSections = True }
noSplitSections f = f { ghcSplitSections = False }

-- | Build GHC and libraries with ThreadSanitizer support. You likely want to
-- configure with @--disable-large-address-space@ when using this.
enableThreadSanitizer :: Bool -> Flavour -> Flavour
enableThreadSanitizer instrumentCmm = addArgs $ notStage0 ? mconcat
    [ instrumentCmm ? builder (Ghc CompileCWithGhc) ? arg "-optc-fsanitize=thread"

    , builder (Ghc LinkHs) ? (arg "-optc-fsanitize=thread" <> arg "-optl-fsanitize=thread")
    , builder Cc ? arg "-fsanitize=thread"
    , builder (Cabal Flags) ? arg "thread-sanitizer"
    , builder Testsuite ? arg "--config=have_thread_sanitizer=True"
    , builder (Ghc CompileHs) ? mconcat
        [ package pkg ? (arg "-optc-fsanitize=thread" <> arg "-fcmm-thread-sanitizer")
        | pkg <- [base, ghcInternal, array, rts]
        ]
    ]

-- | Use the LLVM backend in stages 1 and later.
viaLlvmBackend :: Flavour -> Flavour
viaLlvmBackend = addArgs $ notStage0 ? builder Ghc ? arg "-fllvm"

-- | Build the GHC executable with profiling enabled in stages 2 and later. It
-- is also recommended that you use this with @'dynamicGhcPrograms' = False@
-- since GHC does not support loading of profiled libraries with the
-- dynamically-linker.
enableProfiledGhc :: Flavour -> Flavour
enableProfiledGhc flavour =
  enableLateCCS flavour
    { rtsWays = do
        ws <- rtsWays flavour
        mconcat
          [ pure ws
          , buildingCompilerStage' (>= Stage2) ? pure (foldMap profiled_ways ws)
          ]
    , libraryWays = mconcat
        [ libraryWays flavour
        , buildingCompilerStage' (>= Stage2) ? pure (Set.singleton profiling)
        ]
    , ghcProfiled = (>= Stage2)
    }
    where
      profiled_ways w
        | wayUnit Dynamic w = Set.empty
        | otherwise         = Set.singleton (w <> profiling)

-- | Disable 'dynamicGhcPrograms'.
disableDynamicGhcPrograms :: Flavour -> Flavour
disableDynamicGhcPrograms flavour = flavour { dynamicGhcPrograms = pure False }

-- | Don't build libraries in dynamic 'Way's.
disableDynamicLibs :: Flavour -> Flavour
disableDynamicLibs flavour =
  flavour { libraryWays = prune $ libraryWays flavour,
            rtsWays = prune $ rtsWays flavour,
            dynamicGhcPrograms = pure False
          }
  where
    prune :: Ways -> Ways
    prune = fmap $ Set.filter (not . wayUnit Dynamic)


-- | Don't build libraries in profiled 'Way's.
disableProfiledLibs :: Flavour -> Flavour
disableProfiledLibs flavour =
    flavour { libraryWays = prune $ libraryWays flavour
            , rtsWays     = prune $ rtsWays flavour
            }
  where
    prune :: Ways -> Ways
    prune = fmap $ Set.filter (not . wayUnit Profiling)

useNativeBignum :: Flavour -> Flavour
useNativeBignum flavour =
  flavour { bignumBackend = "native"
          }

-- | Enable building the @text@ package with @simdutf@ support.
enableTextWithSIMDUTF :: Flavour -> Flavour
enableTextWithSIMDUTF flavour = flavour {
  textWithSIMDUTF = True
}

-- | Build stage2 compiler with -fomit-interface-pragmas to reduce
-- recompilation.
omitPragmas :: Flavour -> Flavour
omitPragmas = addArgs
    $ notStage0 ? builder (Ghc CompileHs) ? package compiler
    ? arg "-fomit-interface-pragmas"

-- | Build stage2 dependencies with options to enable IPE debugging
-- information.
enableIPE :: Flavour -> Flavour
enableIPE = addArgs
    $ notStage0 ? builder (Ghc CompileHs)
    ? pure ["-finfo-table-map", "-fdistinct-constructor-tables"]

enableLateCCS :: Flavour -> Flavour
enableLateCCS = addArgs
  $ notStage0 ? builder (Ghc CompileHs)
  ? ((Profiling `wayUnit`) <$> getWay)
  ? arg "-fprof-late"

-- | Enable assertions for the stage2 compiler
enableAssertions :: Flavour -> Flavour
enableAssertions flav = flav { ghcDebugAssertions = f }
  where
    f Stage2 = True
    f st = ghcDebugAssertions flav st

-- | Build the stage3 compiler using the non-moving GC.
enableBootNonmovingGc :: Flavour -> Flavour
enableBootNonmovingGc = addArgs $ mconcat
    [ stage Stage2 ? builder Ghc
      ? pure ["+RTS", "--nonmoving-gc", "-RTS"]
    ]

-- | Produce fully statically-linked executables and build libraries suitable
-- for static linking.
fullyStatic :: Flavour -> Flavour
fullyStatic flavour =
    addArgs staticExec $ disableDynamicLibs flavour
  where
    staticExec :: Args
    {- Some packages, especially iserv, seem to force a set of build ways,
     - including some that are dynamic (in Rules.BinaryDist).  Trying to
     - build statically and dynamically at the same time breaks the build,
     - so we respect that overriding of the Ways.  Any code that overrides
     - the Ways will need to include a Way that's not explicitly dynamic
     - (like "vanilla").
     -}
    staticExec = mconcat
        {-
         - Disable dynamic linking by the built ghc executable because the
         - statically-linked musl doesn't support dynamic linking, but will
         - try and fail.
         -}
        [ package compiler ? builder (Cabal Flags) ? arg "-dynamic-system-linker"
        {-
         - The final executables don't work unless the libraries linked into
         - it are compiled with "-fPIC."  The PI stands for "position
         - independent" and generates libraries that work when inlined into
         - an executable (where their position is not at the beginning of
         - the file).
         -}
        , builder (Ghc CompileHs) ? pure [ "-fPIC", "-static" ]
        , builder (Ghc CompileCWithGhc) ? pure [ "-fPIC", "-optc", "-static"]
        , builder (Ghc LinkHs) ? pure [ "-optl", "-static" ]
        ]

-- | Ensure stage0 executables and libraries are fully static. Useful
-- for building cross GHC bindists that still contain shared target
-- libraries.
hostFullyStatic :: Flavour -> Flavour
hostFullyStatic flavour =
    addArgs staticExec $ disableDynamicGhcPrograms flavour
  where
    -- Unlike 'fullyStatic', we need to ensure these flags are only
    -- applied to host code.
    staticExec :: Args
    staticExec = stage0 ? mconcat
        [
          builder (Ghc CompileHs) ? pure [ "-fPIC", "-static" ]
        , builder (Ghc CompileCWithGhc) ? pure [ "-fPIC", "-optc", "-static"]
        , builder (Ghc LinkHs) ? pure [ "-optl", "-static" ]
        ]

-- | Build stage2 dependencies with options to enable collection of compiler
-- stats.
collectTimings :: Flavour -> Flavour
collectTimings =
  -- Why both -ddump-timings *and* -v?
  -- In contrast to -ddump-timings, -v will seq the whole CoreProgram and
  -- produce less missleading information; otherwise, due to laziness some
  -- allocations might be attributed to a subsequent pass instead of the pass
  -- that has been causing the allocation. So we want -v.
  -- On the other hand, -v doesn't work with -ddump-to-file, so we need
  -- -ddump-timings.
  addArgs $ notStage0 ? builder (Ghc CompileHs) ?
    pure ["-ddump-to-file", "-ddump-timings", "-v"]

-- | Build ghc with debug rts (i.e. -debug) in and after this stage
debugGhc :: Stage -> Flavour -> Flavour
debugGhc ghcStage f = f
  { ghcDebugged = (>= ghcStage)
  , rtsWays = do
      ws <- rtsWays f
      mconcat
        [ pure ws
        , buildingCompilerStage' (>= ghcStage) ? pure (Set.map (<> debug) ws)
        ]
  }

-- * CLI and <root>/hadrian.settings options

{-
Note [Hadrian settings]
~~~~~~~~~~~~~~~~~~~~~~~
Hadrian lets one customize GHC builds through the UserSettings module,
where Hadrian users can override existing 'Flavour's or create entirely
new ones, overriding/extending the options passed to some builder
building the RTS in more ways and much more.

It now also offers a more "old-school" interface, in the form of
@foo.bar.baz = v@ or @foo.bar.baz += v@ expressions, that one can
pass on the command line that invokes hadrian:

> $ hadrian/build --flavour=quickest -j "stage1.ghc-bin.ghc.link.opts += -v3"

or in a file at <build root>/hadrian.settings, where <build root>
is the build root to be used for the build, which is _build by default.
For example, you could create a file at _build/hadrian.settings with the
following contents:

> stage1.ghc-bin.ghc.link.opts += -v3
> stage1.base.ghc.hs.opts += -ddump-timings

and issue:

> $ hadrian/build

Hadrian would pick up the settings given in _build/hadrian.settings (as well as
any settings that you may additionally be passing on the command line) and
update the relevant flavour accordingly, to issue the additional arguments
specified by the user.

The supported settings are described by 'builderSetting' below, using
operations from Applicative + two new primitives, 'matchString' and
'matchOneOf', that come as members of the 'Match' class. This gives us
a simple but powerful vocabulary to describe settings and parse them
into values that we can use to compute interesting things, like a 'Predicate'
that we can use to emit additional arguments, or a list of possible completions.

> fmap, (<$>) :: Functor f => (a -> b) -> f a -> f b
> pure :: Applicative f => a -> f a
> (<*>) :: Applicative f => f (a -> b) -> f a -> f b
> (*>) :: Applicative f => f a -> f b -> f b
> (<*) :: Applicative f => f a -> f b -> f a
> (<$) :: Functor f => a -> f b -> f a
>
> str :: Match f => String -> f ()
> val :: Match f => String -> a -> f a
> oneOf :: Match f => [f a] -> f a
> choose :: Match f => [(String, a)] -> f a
> wild :: Match f => [(String, a)] -> f (Wildcard a)

For instance, to describe possible settings:
  foo.bar.{x, y}
  foo.baz.{a, b}.c

we could write:

> str "foo" *> oneOf [ str "bar" *> choose [ ("x", "x"), ("y", "y") ]
>                    , str "baz" *> choose [ ("a", "ac"), ("b", "bc") <* str "c" ]
>                    ]

'builderSetting' uses these combinators to describe the setting keys that
Hadrian supports. A user-oriented description of this mechanism is available
in hadrian/doc/user-settings.md.

-}

-- | Try to interpret all the 'KeyVal' as flavour updates, keeping
--   a list of errors for the ones which don't match known
--   settings.
applySettings :: [KeyVal] -> ([SettingError], Flavour -> Flavour)
applySettings kvs = case partitionEithers (map applySetting kvs) of
  (errs, fs) -> (errs, foldr (flip (.)) id fs)
  -- we need to compose the reverse way to have the following settings
  --     x  = -b
  --     x += -c
  -- produce the final x = "-b -c" value. With just (.) we would apply
  -- the x = -b assignment last, which would silently drop the -c addition.
  --
  --     foldr (.) id [f, g, h] = f . g . h
  --        -- first function (f) is applied last, we're applying them in
  --        -- the wrong order!
  --
  --     foldr (flip (.)) id [f, g, h] = h . g . f
  --        -- last function (f) is applied last, as desired


-- | Try to interpret the given 'KeyVal' as a flavour update
--   function, returning an error if it doesn't match a known
--   setting.
applySetting :: KeyVal -> Either SettingError (Flavour -> Flavour)
applySetting (KeyVal ks op v) = case runSettingsM ks builderPredicate of
  Left err -> throwError $
      "error while setting `" ++ intercalate "`." ks ++ ": " ++ err
  Right pred -> Right $ \flav -> flav
    { extraArgs = update (extraArgs flav) pred }

  where override arguments predicate = do
          holds <- predicate
          if holds then pure (words v) else arguments

        augment arguments predicate =
          mconcat [arguments, predicate ? pure (words v)]

        update
          | op == Equal = override
          | otherwise   = augment

-- | Try to auto-complete the given @Key@ using
--   all known settings, as described by 'builderSetting'.
--
-- > completeSetting ["stage1","base", "ghc"]
-- >   -- returns [ ["stage1","base","ghc","c","opts"]
-- >   --         , ["stage1","base","ghc","hs","opts"]
-- >   --         , ["stage1","base","ghc","link","opts"]
-- >   --         , ["stage1","base","ghc","deps","opts"]
-- >   --         , ["stage1","base","ghc","toolargs","opts"]
-- >   --         ]
completeSetting :: Key -> [Key]
completeSetting ks = map snd (complete ks builderSetting)

-- | Interpret a 'builderSetting' as a 'Predicate' that
--   potentially constrains on the stage, package or
--   (ghc or cc) builder mode.
--
--   For example, @stage1.base.ghc.link.opts@ gets mapped to
--   a predicate that applies @'stage' 'Stage1'@,
--   @'package' 'base'@ and @'builder' ('Ghc' 'LinkHs')@.
builderPredicate :: SettingsM Predicate
builderPredicate = builderSetting <&> (\(wstg, wpkg, builderMode) ->
  wildcard (pure True) stage wstg <&&>
  wildcard (pure True) package wpkg <&&>
    (case builderMode of
       BM_Ghc ghcMode -> wildcard (builder Ghc) (builder . Ghc) ghcMode
       BM_Cc  ccMode  -> wildcard (builder Cc) (builder . Cc) ccMode
       BM_CabalConfigure -> builder (Cabal Setup)
       BM_RunTest        -> builder Testsuite
       BM_Hsc2HsRun      -> builder Hsc2Hs
       BM_Haddock        -> builder (Haddock BuildPackage)
    )
  )

  where (<&&>) = liftA2 (&&)

-- | Which builder a setting should apply to
data BuilderMode = BM_Ghc (Wildcard GhcMode)
                 | BM_Cc  (Wildcard CcMode)
                 | BM_CabalConfigure
                 | BM_Hsc2HsRun
                 | BM_RunTest
                 | BM_Haddock

-- | Interpretation-agnostic description of the builder settings
--   supported by Hadrian.
--
--   Supported settings (to be kept in sync with the code):
--
--   > (<stage> or *).(<package name> or *).ghc.(<ghc mode> or *).opts
--   > (<stage> or *).(<package name> or *).cc.(<cc mode> or *).opts
--   > (<stage> or *).(<package name> or *).cabal.configure.opts
--   > (<stage> or *).(<package name> or *).hsc2hs.run.opts
--   > runtest.opts
--
--   where:
--     - @<stage>@ is one of @stage0@, @stage1@, @stage2@ or @stage3@;
--     - @<package name>@ is the (Cabal) name of a package (@base@,
--       @template-haskell@, ...);
--     - @<ghc mode>@ is one of @c@ (building C files), @hs@ (building Haskell
--       modules), @link@ (linking object files), @deps@ (finding Haskell
--       dependencies with @ghc -M@) or @toolargs@ (getting necessary flags to
--       make hadrian/ghci work;
--     - @<cc mode>@ is one of @c@ (building C files) or @deps@ (finding C
--       dependencies);
--     - locations that support a wildcard (@*@) entry are here to avoid
--       repetition, a wildcard entry being equivalent to writing all the
--       settings that the wildcard matches over; in our case, we can
--       apply GHC or C compiler options uniformly over all stages, packages
--       and compiler modes, if we so desire, by using a wildcard in the
--       appropriate spot.
builderSetting :: Match f
               => f (Wildcard Stage, Wildcard Package, BuilderMode)
builderSetting =
    matchOneOf
    [ (,,)
       <$> wild stages
       <*> wild pkgs
       <*> matchOneOf
             [ str "ghc" *> fmap BM_Ghc (wild ghcBuilder) <* str "opts"
             , str "cc" *> fmap BM_Cc (wild ccBuilder) <* str "opts"
             , BM_CabalConfigure <$ str "cabal" <* str "configure" <* str "opts"
             , BM_Hsc2HsRun <$ str "hsc2hs" <* str "run" <* str "opts"
             , BM_Haddock   <$ str "haddock" <* str "build-package" <* str "opts"
             ]
    , (Wildcard, Wildcard, BM_RunTest)
      <$ str "runtest" <* str "opts"
    ]
  where ghcBuilder =
          [ ("c", CompileCWithGhc)
          , ("cpp", CompileCppWithGhc)
          , ("deps", FindHsDependencies)
          , ("hs", CompileHs)
          , ("link", LinkHs)
          , ("toolargs", ToolArgs)
          ]

        ccBuilder =
          [ ("c", CompileC)
          -- Not sure how to handle the FindCDependencies CxxDep case
          , ("deps", FindCDependencies CDep)
          ]

        stages = map (\stg -> (stageString stg, stg)) allStages

        pkgs = map (\pkg -> (pkgName pkg, pkg)) (ghcPackages ++ userPackages)
