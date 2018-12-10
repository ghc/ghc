module Rules.Dependencies (buildPackageDependencies) where

import Data.Bifunctor
import Data.Function

import Base
import Context
import Expression
import Hadrian.BuildPath
import Oracles.ModuleFiles
import Rules.Generate
import Settings.Default
import Target
import Utilities

import qualified Text.Parsec as Parsec

buildPackageDependencies :: [(Resource, Int)] -> Rules ()
buildPackageDependencies rs = do
    root <- buildRootRules
    root -/- "**/.dependencies.mk" %> \mk -> do
        depfile <- getDepMkFile root mk
        context <- depMkFileContext depfile
        srcs <- hsSources context
        need srcs
        orderOnly =<< interpretInContext context generatedDependencies
        if null srcs
        then writeFileChanged mk ""
        else buildWithResources rs $
            target context
                   (Ghc FindHsDependencies $ Context.stage context)
                   srcs [mk]
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


data DepMkFile = DepMkFile Stage FilePath
  deriving (Eq, Show)

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

depMkFileContext :: DepMkFile -> Action Context
depMkFileContext (DepMkFile stage pkgpath) = do
  pkg <- getPackageByPath pkgpath
  return (Context stage pkg vanilla)
