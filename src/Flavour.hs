module Flavour (Flavour (..)) where

import Expression

-- | 'Flavour' is a collection of build settings that fully define a GHC build.
-- Note the following type semantics:
-- * @Bool@: a plain Boolean flag whose value is known at compile time.
-- * @Action Bool@: a flag whose value can depend on the build environment.
-- * @Predicate@: a flag whose value can depend on the build environment and
-- on the current build target.
data Flavour = Flavour {
    -- | Flavour name, to set from command line.
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
    -- | Build Haddock and documentation.
    buildHaddock :: Predicate,
    -- | Build dynamic GHC programs.
    dynamicGhcPrograms :: Bool,
    -- | Enable GHCi debugger.
    ghciWithDebugger :: Bool,
    -- | Build profiled GHC.
    ghcProfiled :: Bool,
    -- | Build GHC with debug information.
    ghcDebugged :: Bool }
