module Hadrian.Haskell.Cabal (readCabal, cabalNameVersion, cabalDependencies) where

import Development.Shake
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Types.CondTree
import Distribution.Verbosity

-- TODO: Track the values?

-- | Read a given @.cabal@ file and return the 'GenericPackageDescription'.
readCabal :: FilePath -> Action GenericPackageDescription
readCabal cabal = do
    need [cabal]
    liftIO $ readGenericPackageDescription silent cabal

-- | Read a given @.cabal@ file and return the package name and version.
cabalNameVersion :: FilePath -> Action (String, String)
cabalNameVersion cabal = do
    identifier <- package . packageDescription <$> readCabal cabal
    return (unPackageName $ pkgName identifier, display $ pkgVersion identifier)

-- | Read a given @.cabal@ file and return the package dependencies.
cabalDependencies :: FilePath -> Action [String]
cabalDependencies cabal = do
    gpd <- readCabal cabal
    let depsLib  = collectDeps $ condLibrary gpd
        depsExes = map (collectDeps . Just . snd) $ condExecutables gpd
        deps     = concat $ depsLib : depsExes
    return $ [ unPackageName name | Dependency name _ <- deps ]

collectDeps :: Maybe (CondTree v [Dependency] a) -> [Dependency]
collectDeps Nothing = []
collectDeps (Just (CondNode _ deps ifs)) = deps ++ concatMap f ifs
  where
    f (CondBranch _ t mt) = collectDeps (Just t) ++ collectDeps mt
