-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Cabal
-- Copyright  : (c) Andrey Mokhov 2014-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Basic functionality for extracting Haskell package metadata stored in
-- Cabal files.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Cabal (
    pkgVersion, pkgIdentifier, pkgSynopsis, pkgDescription, pkgDependencies,
    pkgGenericDescription, cabalArchString, cabalOsString,
    ) where

import Development.Shake
import Distribution.PackageDescription (GenericPackageDescription, ConfVar (Impl, PackageFlag), mkFlagName)

import Hadrian.Haskell.Cabal.Type
import Hadrian.Oracles.Cabal
import Hadrian.Package
import Stage
import Oracles.Setting
import Data.Version.Extra
import qualified Distribution.Types.CondTree as C
import qualified Distribution.Types.Dependency as C
import qualified Distribution.Types.Condition as C
import Data.Maybe
import Data.List.Extra
import qualified Distribution.Simple                           as C

-- | Read a Cabal file and return the package version. The Cabal file is tracked.
pkgVersion :: Package -> Action String
pkgVersion = fmap version . readPackageData

-- | Read a Cabal file and return the package identifier, e.g. @base-4.10.0.0@.
-- The Cabal file is tracked.
pkgIdentifier :: Package -> Action String
pkgIdentifier package = do
    cabal <- readPackageData package
    return $ if null (version cabal)
        then name cabal
        else name cabal ++ "-" ++ version cabal

-- | Read a Cabal file and return the package synopsis. The Cabal file is tracked.
pkgSynopsis :: Package -> Action String
pkgSynopsis = fmap synopsis . readPackageData

-- | Read a Cabal file and return the package description. The Cabal file is
-- tracked.
pkgDescription :: Package -> Action String
pkgDescription = fmap description . readPackageData

-- | Read a Cabal file and return the sorted list of the package dependencies.
-- The current version does not take care of Cabal conditionals and therefore
-- returns a crude overapproximation of actual dependencies. The Cabal file is
-- tracked.
pkgDependencies :: Stage -> Package -> Action [PackageName]
pkgDependencies st pkg =  do
  ghc_ver <- readVersion <$> ghcVersionStage st
  deps <- packageDependenciesConds <$> readPackageData pkg
  let dep_pkgs = resolve_package (C.mkVersion' ghc_ver) deps
  return dep_pkgs

  where
    resolve_package ghc_ver deps =
        let
          allDeps = collectDeps deps
          sorted :: [PackageName]
          sorted  = sort [ C.unPackageName p | C.Dependency p _ _ <- allDeps ]
          final_deps = nubOrd sorted \\ [pkgName pkg]
        in final_deps

      where
        -- Collect an overapproximation of dependencies by ignoring conditionals
        collectDeps :: C.CondTree ConfVar [C.Dependency] a -> [C.Dependency]
        collectDeps ct = simplifyCondTreeAccum resolveConf ct

        resolveConf (Impl C.GHC vr) = Right (C.withinRange ghc_ver vr)
        resolveConf v@(PackageFlag fn) = if fn == mkFlagName "template-haskell-quotes" then (Right (st >= Stage1)) else Left v
        resolveConf v = Left v

-- | Flatten a CondTree.  This will resolve the CondTree by taking all
-- cannot be evaluated, both branches are returned
simplifyCondTreeAccum :: (Show v, Monoid d) =>
                    (v -> Either v Bool)
                 -> C.CondTree v d a
                 -> d
simplifyCondTreeAccum env (C.CondNode _a d ifs) =
    foldl (<>) d $ mapMaybe simplifyIf ifs
  where
    simplifyIf (C.CondBranch cnd t me) =
        case C.simplifyCondition cnd env of
          (C.Lit True, _) -> Just $ simplifyCondTreeAccum env t
          (C.Lit False, _) -> fmap (simplifyCondTreeAccum env) me
          _ -> Just $ (simplifyCondTreeAccum env t) <>
                        fromMaybe mempty (fmap (simplifyCondTreeAccum env) me)


-- | Read a Cabal file and return the 'GenericPackageDescription'. The Cabal
-- file is tracked.
pkgGenericDescription :: Package -> Action GenericPackageDescription
pkgGenericDescription = fmap genericPackageDescription . readPackageData

-- | Cabal's rendering of an architecture as used in its directory structure.
--
-- Inverse of 'Cabal.Distribution.Simple.GHC.ghcArchString'.
cabalArchString :: String -> String
cabalArchString "powerpc"     = "ppc"
cabalArchString "powerpc64"   = "ppc64"
cabalArchString "powerpc64le" = "ppc64"
cabalArchString other         = other

-- | Cabal's rendering of an OS as used in its directory structure.
--
-- Inverse of 'Cabal.Distribution.Simple.GHC.ghcOsString'.
cabalOsString :: String -> String
cabalOsString "mingw32"  = "windows"
cabalOsString "darwin"   = "osx"
cabalOsString "solaris2" = "solaris"
cabalOsString other      = other
