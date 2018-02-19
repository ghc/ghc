-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Cabal.Parse
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Extracting Haskell package metadata stored in Cabal files.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Cabal.Parse (Cabal (..), parseCabal) where

import Data.List.Extra
import Development.Shake
import qualified Distribution.Package                   as C
import qualified Distribution.PackageDescription        as C
import qualified Distribution.PackageDescription.Parsec as C
import qualified Distribution.Text                      as C
import qualified Distribution.Types.CondTree            as C
import qualified Distribution.Verbosity                 as C

import Hadrian.Haskell.Cabal.Type

-- | Parse a Cabal file.
parseCabal :: FilePath -> IO Cabal
parseCabal file = do
    gpd <- liftIO $ C.readGenericPackageDescription C.silent file
    let pd      = C.packageDescription gpd
        pkgId   = C.package pd
        name    = C.unPackageName (C.pkgName pkgId)
        version = C.display (C.pkgVersion pkgId)
        libDeps = collectDeps (C.condLibrary gpd)
        exeDeps = map (collectDeps . Just . snd) (C.condExecutables gpd)
        allDeps = concat (libDeps : exeDeps)
        sorted  = sort [ C.unPackageName p | C.Dependency p _ <- allDeps ]
        deps    = nubOrd sorted \\ [name]
    return $ Cabal deps name (C.synopsis pd) version

collectDeps :: Maybe (C.CondTree v [C.Dependency] a) -> [C.Dependency]
collectDeps Nothing = []
collectDeps (Just (C.CondNode _ deps ifs)) = deps ++ concatMap f ifs
  where
    f (C.CondBranch _ t mt) = collectDeps (Just t) ++ collectDeps mt
