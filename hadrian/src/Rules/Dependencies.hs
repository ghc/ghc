{-# OPTIONS_GHC -Wno-deprecations #-}

module Rules.Dependencies (buildPackageDependencies) where

import Data.Bifunctor
import qualified Data.List.NonEmpty as NE

import Base
import Context
import Expression
import Hadrian.BuildPath
import Oracles.ModuleFiles
import Rules.Generate
import Settings
import Target
import Utilities
import Packages
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Text.Parsec as Parsec


data PkgMod = PkgMod { pkg :: Package, _mod :: String }

extraDepsList :: [(PkgMod, PkgMod)]
extraDepsList =
    [ (containers, "Data.IntSet.Internal") --> th_internal
    , (containers, "Data.Set.Internal") --> th_internal
    , (containers, "Data.Sequence.Internal") --> th_internal
    , (containers, "Data.Graph") --> th_internal
    ]
  where
    (p1,m1) --> (p2,m2) = (PkgMod p1 m1, PkgMod p2 m2)
    th_internal = (templateHaskell, "Language.Haskell.TH.Lib.Internal")

-- These modules use DeriveLift which needs Language.Haskell.TH.Lib.Internal but
-- the dependency is implicit. ghc -M should emit this additional dependency but
-- until it does we need to add this dependency ourselves.
extraDependenciesFor :: Stage -> Package -> Action [(FilePath, FilePath)]
extraDependenciesFor stage srcPkg
  | Just deps <- M.lookup srcPkg byPackage = concat <$> traverse dep deps
  | otherwise = return []
  where
    byPackage :: M.Map Package [(PkgMod, PkgMod)]
    byPackage = M.fromListWith (++) [ (pkg x, [(x,y)]) | (x,y) <- extraDepsList ]

    -- @dep ((p1, m1), (p2, m2))@ is an extra dependency from
    -- module m1 of package p1 to module m2 of package p2.
    dep :: (PkgMod, PkgMod) -> Action [(FilePath, FilePath)]
    dep (PkgMod p1 m1, PkgMod p2 m2) = do
        let context = Context stage p1 (error "extra_dependencies: way not set") (error "extra_dependencies: inplace not set")
        ways <- interpretInContext context getLibraryWays
        mapM (\way -> (,) <$> path way p1 m1 <*> path way p2 m2) (S.toList ways)

    path :: Way -> Package -> String -> Action FilePath
    path way p m =
      let context = Context stage p way Inplace
      in objectPath context . moduleSource $ m

formatExtra :: (FilePath, FilePath) -> String
formatExtra (fp1, fp2) = fp1 ++ ":" ++ fp2 ++ "\n"

buildPackageDependencies :: [(Resource, Int)] -> Rules ()
buildPackageDependencies rs = do
    root <- buildRootRules
    root -/- "**/.dependencies.mk" %> \mk -> do
        DepMkFile stage pkgpath <- getDepMkFile root mk
        let pkg = unsafeFindPackageByPath pkgpath
            context = Context stage pkg vanilla Inplace
        extra <- extraDependenciesFor stage pkg
        srcs <- hsSources context
        gens <- interpretInContext context generatedDependencies
        need (srcs ++ gens)
        if null srcs
        then writeFileChanged mk ""
        else buildWithResources rs $ target context
            (Ghc FindHsDependencies $ Context.stage context) srcs [mk]
        liftIO $ mapM_ (appendFile mk . formatExtra) extra
        removeFile $ mk <.> "bak"

    root -/- "**/.dependencies" %> \deps -> do
        mkDeps <- readFile' (deps <.> "mk")
        writeFileChanged deps . unlines
                              . map (\(src, deps) -> unwords $ src : deps)
                              . map (bimap unifyPath (map unifyPath))
                              . map (bimap NE.head concat . NE.unzip)
                              . NE.groupAllWith fst
                              $ parseMakefile mkDeps


data DepMkFile = DepMkFile Stage FilePath deriving (Eq, Show)

parseDepMkFile :: FilePath -> Parsec.Parsec String () DepMkFile
parseDepMkFile root = do
    _ <- Parsec.string root *> Parsec.optional (Parsec.char '/')
    stage <- parseStage
    _ <- Parsec.char '/'
    pkgPath <- Parsec.manyTill Parsec.anyChar
        (Parsec.try $ Parsec.string "/.dependencies.mk")
    return (DepMkFile stage pkgPath)

getDepMkFile :: FilePath -> FilePath -> Action DepMkFile
getDepMkFile root = parsePath (parseDepMkFile root) "<dependencies file>"
