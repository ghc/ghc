module Rules.Dependencies (buildPackageDependencies) where

import Development.Shake.Util

import Base
import Context
import Expression
import Oracles.ModuleFiles
import Rules.Actions
import Settings.Paths
import Target

buildPackageDependencies :: [(Resource, Int)] -> Context -> Rules ()
buildPackageDependencies rs context@Context {..} =
    buildPath context -/- ".dependencies" %> \deps -> do
        srcs <- haskellSources context
        need srcs
        let mk = deps <.> "mk"
        if srcs == []
        then writeFileChanged mk ""
        else buildWithResources rs $
            Target context (Ghc FindHsDependencies stage) srcs [mk]
        removeFile $ mk <.> "bak"
        mkDeps <- readFile' mk
        writeFileChanged deps . unlines
                              . map (\(src, deps) -> unwords $ src : deps)
                              . map (bimap unifyPath (map unifyPath))
                              . map (bimap head concat . unzip)
                              . groupBy ((==) `on` fst)
                              . sortBy (compare `on` fst)
                              $ parseMakefile mkDeps
