module Flavour (Flavour (..)) where

import Expression

-- | 'Flavour' is a collection of build settings that fully define a GHC build.
-- Note the following type semantics:
-- * @Bool@: a plain Boolean flag whose value is known at compile time.
-- * @Action Bool@: a flag whose value can depend on the build environment.
-- * @Predicate@: a flag whose value can depend on the build environment and
-- on the current build target.
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
