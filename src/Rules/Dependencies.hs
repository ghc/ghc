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
buildPackageDependencies rs context@Context {..} =
    "//" ++ contextDir context -/- ".dependencies" %> \deps -> do
        srcs <- hsSources context
        need srcs
        orderOnly =<< interpretInContext context generatedDependencies
        let mk = deps <.> "mk"
        if null srcs
        then writeFile' mk ""
        else buildWithResources rs $
            target context (Ghc FindHsDependencies stage) srcs [mk]
        removeFile $ mk <.> "bak"
        mkDeps <- liftIO $ readFile mk
        writeFileChanged deps . unlines
                              . map (\(src, deps) -> unwords $ src : deps)
                              . map (bimap unifyPath (map unifyPath))
                              . map (bimap head concat . unzip)
                              . groupBy ((==) `on` fst)
                              . sortBy (compare `on` fst)
                              $ parseMakefile mkDeps
