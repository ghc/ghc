-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Cabal.Parse
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Extracting Haskell package metadata stored in @.cabal@ files.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Cabal.Parse (Cabal (..), parseCabal) where

import Data.List.Extra
import Development.Shake
import Development.Shake.Classes
import qualified Distribution.Package                  as C
import qualified Distribution.PackageDescription       as C
import qualified Distribution.PackageDescription.Parse as C
import qualified Distribution.Text                     as C
import qualified Distribution.Types.CondTree           as C
import qualified Distribution.Verbosity                as C

import Hadrian.Haskell.Package

-- TODO: Use fine-grain tracking instead of tracking the whole @.cabal@ file.
-- | Haskell package metadata extracted from a @.cabal@ file.
data Cabal = Cabal
    { dependencies :: [PackageName]
    , name         :: PackageName
    , version      :: String
    } deriving (Eq, Read, Show, Typeable)

instance Binary Cabal where
    put = put . show
    get = fmap read get

instance Hashable Cabal where
    hashWithSalt salt = hashWithSalt salt . show

instance NFData Cabal where
    rnf (Cabal a b c) = a `seq` b `seq` c `seq` ()

-- | Parse a @.cabal@ file.
parseCabal :: FilePath -> IO Cabal
parseCabal file = do
    gpd <- liftIO $ C.readGenericPackageDescription C.silent file
    let pkgId   = C.package (C.packageDescription gpd)
        name    = C.unPackageName (C.pkgName pkgId)
        version = C.display (C.pkgVersion pkgId)
        libDeps = collectDeps (C.condLibrary gpd)
        exeDeps = map (collectDeps . Just . snd) (C.condExecutables gpd)
        allDeps = concat (libDeps : exeDeps)
        sorted  = sort [ C.unPackageName p | C.Dependency p _ <- allDeps ]
        deps    = nubOrd sorted \\ [name]
    return $ Cabal deps name version

collectDeps :: Maybe (C.CondTree v [C.Dependency] a) -> [C.Dependency]
collectDeps Nothing = []
collectDeps (Just (C.CondNode _ deps ifs)) = deps ++ concatMap f ifs
  where
    f (C.CondBranch _ t mt) = collectDeps (Just t) ++ collectDeps mt
