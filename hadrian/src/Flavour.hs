module Flavour
  ( Flavour (..), werror
  , DocTargets, DocTarget(..)
  , parseFlavour
    -- * Flavour transformers
  , flavourTransformers
  , addArgs
  , splitSections, splitSectionsIf
  , enableThreadSanitizer
  , enableDebugInfo, enableTickyGhc
  , viaLlvmBackend
  , enableProfiledGhc
  , disableDynamicGhcPrograms
  , disableProfiledLibs
  ) where

import Expression
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M
import Packages

import Text.Parsec.Prim as P
import Text.Parsec.Combinator as P
import Text.Parsec.Char as P

-- Please update doc/{flavours.md, user-settings.md} when changing this file.
-- | 'Flavour' is a collection of build settings that fully define a GHC build.
-- Note the following type semantics:
-- * @Bool@: a plain Boolean flag whose value is known at compile time.
-- * @Action Bool@: a flag whose value can depend on the build environment.
-- * @Predicate@: a flag whose value can depend on the build environment and
-- on the current build target.
data Flavour = Flavour {
    -- | Flavour name, to select this flavour from command line.
    name :: String,
    -- | Use these command line arguments.
    args :: Args,
    -- | Build these packages.
    packages :: Stage -> Action [Package],
    -- | 'native', 'gmp', 'ffi'.
    bignumBackend :: String,
    -- | Check selected backend against native backend
    bignumCheck :: Bool,
    -- | Build libraries these ways.
    libraryWays :: Ways,
    -- | Build RTS these ways.
    rtsWays :: Ways,
    -- | Build dynamic GHC programs.
    dynamicGhcPrograms :: Action Bool,
    -- | Enable GHCi debugger.
    ghciWithDebugger :: Bool,
    -- | Build profiled GHC.
    ghcProfiled :: Bool,
    -- | Build GHC with debugging assertions.
    ghcDebugged :: Bool,
    -- | Build the GHC executable against the threaded runtime system.
    ghcThreaded :: Bool,
    -- | Whether to build docs and which ones
    --   (haddocks, user manual, haddock manual)
    ghcDocs :: Action DocTargets }

-- | A set of documentation targets
type DocTargets = Set DocTarget

-- | Documentation targets
--
--   While we can't reasonably expose settings or CLI options
--   to selectively disable, say, base's haddocks, we can offer
--   a less fine-grained choice:
--
--   - haddocks for libraries
--   - non-haddock html pages (e.g GHC's user manual)
--   - PDF documents (e.g haddock's manual)
--   - man pages (GHC's)
--
--   The main goal being to have easy ways to do away with the need
--   for e.g @sphinx-build@ or @xelatex@ and associated packages
--   while still being able to build a(n almost) complete binary
--   distribution.
data DocTarget = Haddocks | SphinxHTML | SphinxPDFs | SphinxMan | SphinxInfo
  deriving (Eq, Ord, Show, Bounded, Enum)

flavourTransformers :: Map String (Flavour -> Flavour)
flavourTransformers = M.fromList
    [ "werror" =: werror
    , "debug_info" =: enableDebugInfo
    , "ticky_ghc" =: enableTickyGhc
    , "split_sections" =: splitSections
    , "thread_sanitizer" =: enableThreadSanitizer
    , "llvm" =: viaLlvmBackend
    , "profiled_ghc" =: enableProfiledGhc
    , "no_dynamic_ghc" =: disableDynamicGhcPrograms
    , "no_profiled_libs" =: disableProfiledLibs
    , "ipe" =: enableIPE
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
                 ]      -- needed to parse e.g. "quick-debug" before "quick"

    flavourTrans :: Parser (Flavour -> Flavour)
    flavourTrans = do
        void $ P.char '+'
        P.choice [ trans <$ P.try (P.string nm)
                 | (nm, trans) <- M.toList transformers
                 ]

-- | Add arguments to the 'args' of a 'Flavour'.
addArgs :: Args -> Flavour -> Flavour
addArgs args' fl = fl { args = args fl <> args' }

-- | Turn on -Werror for packages built with the stage1 compiler.
-- It mimics the CI settings so is useful to turn on when developing.
werror :: Flavour -> Flavour
werror = addArgs (builder Ghc ? notStage0 ? arg "-Werror")

-- | Build C and Haskell objects with debugging information.
enableDebugInfo :: Flavour -> Flavour
enableDebugInfo = addArgs $ notStage0 ? mconcat
    [ builder (Ghc CompileHs) ? arg "-g3"
    , builder (Cc CompileC) ? arg "-g3"
    , builder (Cabal Setup) ? arg "--disable-library-stripping"
    , builder (Cabal Setup) ? arg "--disable-executable-stripping"
    ]

-- | Enable the ticky-ticky profiler in stage2 GHC
enableTickyGhc :: Flavour -> Flavour
enableTickyGhc =
    addArgs $ stage1 ? mconcat
      [ builder (Ghc CompileHs) ? ticky
      , builder (Ghc LinkHs) ? ticky
      ]
  where
    ticky = mconcat
      [ arg "-ticky"
      , arg "-ticky-allocd"
      , arg "-ticky-dyn-thunk"
      -- You generally need STG dumps to interpret ticky profiles
      , arg "-ddump-to-file"
      , arg "-ddump-stg-final"
      ]

-- | Transform the input 'Flavour' so as to build with
--   @-split-sections@ whenever appropriate. You can
--   select which package gets built with split sections
--   by passing a suitable predicate. If the predicate holds
--   for a given package, then @split-sections@ is used when
--   building it. If the given flavour doesn't build
--   anything in a @dyn@-enabled way, then 'splitSections' is a no-op.
splitSectionsIf :: (Package -> Bool) -> Flavour -> Flavour
splitSectionsIf pkgPredicate = addArgs $ do
    way <- getWay
    pkg <- getPackage
    (Dynamic `wayUnit` way) ? pkgPredicate pkg ?
        builder (Ghc CompileHs) ? arg "-split-sections"

-- | Like 'splitSectionsIf', but with a fixed predicate: use
--   split sections for all packages but the GHC library.
splitSections :: Flavour -> Flavour
splitSections = splitSectionsIf (/=ghc)
-- Disable section splitting for the GHC library. It takes too long and
-- there is little benefit.

enableThreadSanitizer :: Flavour -> Flavour
enableThreadSanitizer = addArgs $ mconcat
    [ builder (Ghc CompileHs) ? arg "-optc-fsanitize=thread"
    , builder (Ghc CompileCWithGhc) ? (arg "-optc-fsanitize=thread" <> arg "-DTSAN_ENABLED")
    , builder (Ghc LinkHs) ? arg "-optl-fsanitize=thread"
    , builder (Cc  CompileC) ? (arg "-fsanitize=thread" <> arg "-DTSAN_ENABLED")
    , builder (Cabal Flags) ? arg "thread-sanitizer"
    , builder  RunTest ? arg "--config=have_thread_sanitizer=True"
    ]

-- | Use the LLVM backend in stages 1 and later.
viaLlvmBackend :: Flavour -> Flavour
viaLlvmBackend = addArgs $ notStage0 ? builder Ghc ? arg "-fllvm"

-- | Build the GHC executable with profiling enabled. It is also recommended
-- that you use this with @'dynamicGhcPrograms' = False@ since GHC does not
-- support loading of profiled libraries with the dynamically-linker.
enableProfiledGhc :: Flavour -> Flavour
enableProfiledGhc flavour =
    flavour { rtsWays = addWays [profiling, threadedProfiling, debugProfiling, threadedDebugProfiling] (rtsWays flavour)
            , libraryWays = addWays [profiling] (libraryWays flavour)
            , ghcProfiled = True
            }
  where
    addWays :: [Way] -> Ways -> Ways
    addWays ways =
      fmap (++ ways)

-- | Disable 'dynamicGhcPrograms'.
disableDynamicGhcPrograms :: Flavour -> Flavour
disableDynamicGhcPrograms flavour = flavour { dynamicGhcPrograms = pure False }

-- | Don't build libraries in profiled 'Way's.
disableProfiledLibs :: Flavour -> Flavour
disableProfiledLibs flavour =
    flavour { libraryWays = filter (not . wayUnit Profiling) <$> libraryWays flavour }

-- | Build stage2 dependencies with options to enable IPE debugging
-- information.
enableIPE :: Flavour -> Flavour
enableIPE = addArgs
    $ notStage0 ? builder (Ghc CompileHs) ? pure ["-finfo-table-map", "-fdistinct-constructor-tables"]
