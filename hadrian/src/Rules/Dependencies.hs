module Rules.Dependencies (buildPackageDependencies) where

import Data.Bifunctor
import Data.Function

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

import qualified Text.Parsec as Parsec

-- These modules use DeriveLift which needs Language.Haskell.TH.Lib.Internal but
-- the dependency is implicit. ghc -M should emit this additional dependency but
-- until it does we need to add this dependency ourselves.
extra_dependencies :: M.Map Package (Stage -> Action [(FilePath, FilePath)])
extra_dependencies =
  M.fromList [(containers, fmap sequence (sequence
    [dep (containers, "Data.IntSet.Internal") th_internal
    ,dep (containers, "Data.Set.Internal") th_internal
    ,dep (containers, "Data.Sequence.Internal") th_internal
    ,dep (containers, "Data.Graph") th_internal
    ]))
    ]

  where
    th_internal = (templateHaskell, "Language.Haskell.TH.Lib.Internal")
    dep (p1, m1) (p2, m2) s = (,) <$> path s p1 m1 <*> path s p2 m2
    path stage p m =
      let context = Context stage p vanilla Inplace
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
        extra <- maybe (return []) ($ stage) $ M.lookup pkg extra_dependencies
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
                              . map (bimap head concat . unzip)
                              . groupBy ((==) `on` fst)
                              . sortBy (compare `on` fst)
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
