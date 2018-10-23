module Rules.Dependencies (buildPackageDependencies) where

import Data.Bifunctor
import Data.Function

import Base
import Context
import Expression
import Oracles.ModuleFiles
import Rules.Generate
import Target
import Utilities

buildPackageDependencies :: [(Resource, Int)] -> Context -> Rules ()
buildPackageDependencies rs context@Context {..} = do
    root <- buildRootRules
    root -/- contextDir context -/- ".dependencies.mk" %> \mk -> do
        srcs <- hsSources context
        need srcs
        orderOnly =<< interpretInContext context generatedDependencies
        if null srcs
        then writeFileChanged mk ""
        else buildWithResources rs $
            target context (Ghc FindHsDependencies stage) srcs [mk]
        removeFile $ mk <.> "bak"

    root -/- contextDir context -/- ".dependencies" %> \deps -> do
        mkDeps <- readFile' (deps <.> "mk")
        writeFileChanged deps . unlines
                              . map (\(src, deps) -> unwords $ src : deps)
                              . map (bimap unifyPath (map unifyPath))
                              . map (bimap head concat . unzip)
                              . groupBy ((==) `on` fst)
                              . sortBy (compare `on` fst)
                              $ parseMakefile mkDeps
