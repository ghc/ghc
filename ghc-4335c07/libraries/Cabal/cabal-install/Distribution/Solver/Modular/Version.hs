module Distribution.Solver.Modular.Version
    ( Ver
    , VR
    , anyVR
    , checkVR
    , eqVR
    , showVer
    , showVR
    , simplifyVR
    , (.&&.)
    , (.||.)
    ) where

import qualified Distribution.Version as CV -- from Cabal
import Distribution.Text -- from Cabal

-- | Preliminary type for versions.
type Ver = CV.Version

-- | String representation of a version.
showVer :: Ver -> String
showVer = display

-- | Version range. Consists of a lower and upper bound.
type VR = CV.VersionRange

-- | String representation of a version range.
showVR :: VR -> String
showVR = display

-- | Unconstrained version range.
anyVR :: VR
anyVR = CV.anyVersion

-- | Version range fixing a single version.
eqVR :: Ver -> VR
eqVR = CV.thisVersion

-- | Intersect two version ranges.
(.&&.) :: VR -> VR -> VR
(.&&.) = CV.intersectVersionRanges

-- | Union of two version ranges.
(.||.) :: VR -> VR -> VR
(.||.) = CV.unionVersionRanges

-- | Simplify a version range.
simplifyVR :: VR -> VR
simplifyVR = CV.simplifyVersionRange

-- | Checking a version against a version range.
checkVR :: VR -> Ver -> Bool
checkVR = flip CV.withinRange
