module Settings.Flavours.Static (staticFlavour) where

import Expression
import Flavour
import Packages
import {-# SOURCE #-} Settings.Default

import Settings.Flavours.Performance (performanceArgs)

-- Please update doc/flavours.md when changing this file.

-- |Produce statically-linked executables.  Also compiles libraries
-- suitable for static linking.
staticFlavour :: Flavour
staticFlavour = defaultFlavour
    { name = "static"
    , args = defaultBuilderArgs <> performanceArgs <> defaultPackageArgs <> staticExec
    , dynamicGhcPrograms = return False
    , libraryWays = prune $ libraryWays defaultFlavour
    , rtsWays = prune $ rtsWays defaultFlavour
    }

-- Remove any Way that contains a WayUnit of Dynamic
prune :: Ways -> Ways
prune = fmap $ filter staticCompatible

staticCompatible :: Way -> Bool
staticCompatible = not . wayUnit Dynamic

staticExec :: Args
{- Some packages, especially iserv, seem to force a set of build ways,
 - including some that are dynamic (in Rules.BinaryDist).  Trying to
 - build statically and dynamically at the same time breaks the build,
 - so we respect that overriding of the Ways.  Any code that overrides
 - the Ways will need to include a Way that's not explicitly dynamic
 - (like "vanilla").
 -}
staticExec = staticCompatible <$> getWay ? mconcat
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
