module Flavour.Type where

import Expression
import Data.Set (Set)


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
    -- | Use these extra command line arguments.
    -- This can't depend on the result of configuring a package (ie, using readContextData)
    extraArgs :: Args,
    -- | Build these packages.
    packages :: Stage -> Action [Package],
    -- | Bignum backend: 'native', 'gmp', 'ffi', etc.
    bignumBackend :: String,
    -- | Check selected bignum backend against native backend
    bignumCheck :: Bool,
    -- | Build the @text@ package with @simdutf@ support. Disabled by
    -- default due to packaging difficulties described in #20724.
    textWithSIMDUTF :: Stage -- ^ stage of the /built/ compiler
                    -> Bool,
    -- | Build libraries these ways.
    libraryWays :: Ways,
    -- | Build RTS these ways.
    rtsWays :: Ways,
    -- | Build dynamic GHC programs.
    dynamicGhcPrograms :: Stage -> Action Bool,
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

    ghcSplitSections :: Bool, -- ^ Whether to enable split sections
    -- | Whether to build docs and which ones
    --   (haddocks, user manual, haddock manual)
    ghcDocs :: Action DocTargets,

    -- | Whether to uses hashes or inplace for unit ids
    hashUnitIds :: Bool

    }

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
