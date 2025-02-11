module Flavour.Type where

import Expression
import Data.Set (Set)
import Flavour.DocTargets


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
    textWithSIMDUTF :: Bool,
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

    ghcSplitSections :: Bool, -- ^ Whether to enable split sections
    -- | Whether to build docs and which ones
    --   (haddocks, user manual, haddock manual)
    ghcDocs :: Action DocTargets }

