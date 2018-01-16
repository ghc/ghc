-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.PackageUtils
-- Copyright   :  (c) Duncan Coutts 2010
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Various package description utils that should be in the Cabal lib
-----------------------------------------------------------------------------
module Distribution.Client.PackageUtils (
    externalBuildDepends,
  ) where

import Distribution.Package
         ( packageVersion, packageName )
import Distribution.Types.Dependency
import Distribution.Types.UnqualComponentName
import Distribution.PackageDescription
         ( PackageDescription(..), libName )
import Distribution.Version
         ( withinRange, isAnyVersion )

-- | The list of dependencies that refer to external packages
-- rather than internal package components.
--
externalBuildDepends :: PackageDescription -> [Dependency]
externalBuildDepends pkg = filter (not . internal) (buildDepends pkg)
  where
    -- True if this dependency is an internal one (depends on a library
    -- defined in the same package).
    internal (Dependency depName versionRange) =
           (depName == packageName pkg &&
            packageVersion pkg `withinRange` versionRange) ||
           (Just (packageNameToUnqualComponentName depName) `elem` map libName (subLibraries pkg) &&
            isAnyVersion versionRange)
