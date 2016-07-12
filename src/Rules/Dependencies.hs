module Rules.Dependencies (buildPackageDependencies) where

import Development.Shake.Util (parseMakefile)

import Base
import Context
import Expression
import Oracles.ModuleFiles
import Oracles.PackageData
import Rules.Actions
import Settings.Paths
import Target
import UserSettings

buildPackageDependencies :: [(Resource, Int)] -> Context -> Rules ()
buildPackageDependencies rs context@Context {..} =
    let path     = buildPath context
        hDepFile = path -/- ".hs-dependencies"
    in do
        fmap (path ++)
            [ "//*.c.deps", "//*.cmm.deps", "//*.S.deps" ] |%> \out -> do
                let src = dep2src context out
                need [src]
                build $ Target context (Cc FindDependencies stage) [src] [out]

        hDepFile %> \out -> do
            srcs <- haskellSources context
            need srcs
            if srcs == []
            then writeFileChanged out ""
            else buildWithResources rs $
                Target context (Ghc FindDependencies stage) srcs [out]
            removeFile $ out <.> "bak"

        -- TODO: don't accumulate *.deps into .dependencies
        path -/- ".dependencies" %> \out -> do
            cSrcs <- pkgDataList $ CSrcs path
            let cDepFiles = map (src2dep context) cSrcs
            need $ hDepFile : cDepFiles -- need all for more parallelism
            cDeps <- concatMapM readFile' cDepFiles
            hDeps <- readFile' hDepFile
            let result = unlines
                       . map (\(src, deps) -> unwords $ src : deps)
                       . map (bimap unifyPath (map unifyPath))
                       . map (bimap head concat . unzip)
                       . groupBy ((==) `on` fst)
                       . sortBy (compare `on` fst)
                       . parseMakefile $ cDeps ++ hDeps
            writeFileChanged out result

-- Given a 'Context' and a 'FilePath' to a source file, compute the 'FilePath'
-- to its dependencies. For example, in vanillaContext Stage1 rts:
-- * "Task.c"                          -> "_build/stage1/rts/Task.c.deps"
-- * "_build/stage1/rts/AutoApply.cmm" -> "_build/stage1/rts/AutoApply.cmm.deps"
src2dep :: Context -> FilePath -> FilePath
src2dep context src
    | buildRootPath `isPrefixOf` src = src <.> "deps"
    | otherwise                      = buildPath context -/- src <.> "deps"

-- Given a 'Context' and a 'FilePath' to a file with dependencies, compute the
-- 'FilePath' to the source file. For example, in vanillaContext Stage1 rts:
-- * "_build/stage1/rts/Task.c.deps"        -> "Task.c"
-- * "_build/stage1/rts/AutoApply.cmm.deps" -> "_build/stage1/rts/AutoApply.cmm"
dep2src :: Context -> FilePath -> FilePath
dep2src context@Context {..} dep
    | takeBaseName dep `elem` [ "AutoApply.cmm", "Evac_thr.c", "Scav_thr.c" ] = src
    | otherwise = pkgPath package ++ drop (length $ buildPath context) src
  where
    src = dropExtension dep
