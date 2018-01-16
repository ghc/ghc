{-# LANGUAGE DeriveFunctor #-}
module Distribution.Solver.Modular.Package
  ( I(..)
  , Loc(..)
  , PackageId
  , PackageIdentifier(..)
  , PackageName, mkPackageName, unPackageName
  , PkgconfigName, mkPkgconfigName, unPkgconfigName
  , PI(..)
  , PN
  , QPV
  , instI
  , makeIndependent
  , primaryPP
  , setupPP
  , showI
  , showPI
  , unPN
  ) where

import Data.List as L

import Distribution.Package -- from Cabal
import Distribution.Text (display)

import Distribution.Solver.Modular.Version
import Distribution.Solver.Types.PackagePath

-- | A package name.
type PN = PackageName

-- | Unpacking a package name.
unPN :: PN -> String
unPN = unPackageName

-- | Package version. A package name plus a version number.
type PV = PackageId

-- | Qualified package version.
type QPV = Qualified PV

-- | Package id. Currently just a black-box string.
type PId = UnitId

-- | Location. Info about whether a package is installed or not, and where
-- exactly it is located. For installed packages, uniquely identifies the
-- package instance via its 'PId'.
--
-- TODO: More information is needed about the repo.
data Loc = Inst PId | InRepo
  deriving (Eq, Ord, Show)

-- | Instance. A version number and a location.
data I = I Ver Loc
  deriving (Eq, Ord, Show)

-- | String representation of an instance.
showI :: I -> String
showI (I v InRepo)   = showVer v
showI (I v (Inst uid)) = showVer v ++ "/installed" ++ shortId uid
  where
    -- A hack to extract the beginning of the package ABI hash
    shortId = snip (splitAt 4) (++ "...")
            . snip ((\ (x, y) -> (reverse x, y)) . break (=='-') . reverse) ('-':)
            . display
    snip p f xs = case p xs of
                    (ys, zs) -> (if L.null zs then id else f) ys

-- | Package instance. A package name and an instance.
data PI qpn = PI qpn I
  deriving (Eq, Ord, Show, Functor)

-- | String representation of a package instance.
showPI :: PI QPN -> String
showPI (PI qpn i) = showQPN qpn ++ "-" ++ showI i

instI :: I -> Bool
instI (I _ (Inst _)) = True
instI _              = False

-- | Is the package in the primary group of packages.  This is used to
-- determine (1) if we should try to establish stanza preferences
-- for this goal, and (2) whether or not a user specified @--constraint@
-- should apply to this dependency (grep 'primaryPP' to see the
-- use sites).  In particular this does not include packages pulled in
-- as setup deps.
--
primaryPP :: PackagePath -> Bool
primaryPP (PackagePath _ns q) = go q
  where
    go QualToplevel    = True
    go (QualBase  _)   = True
    go (QualSetup _)   = False
    go (QualExe _ _)   = False

-- | Is the package a dependency of a setup script.  This is used to
-- establish whether or not certain constraints should apply to this
-- dependency (grep 'setupPP' to see the use sites).
--
setupPP :: PackagePath -> Bool
setupPP (PackagePath _ns (QualSetup _)) = True
setupPP (PackagePath _ns _)         = False

-- | Qualify a target package with its own name so that its dependencies are not
-- required to be consistent with other targets.
makeIndependent :: PN -> QPN
makeIndependent pn = Q (PackagePath (Independent pn) QualToplevel) pn
