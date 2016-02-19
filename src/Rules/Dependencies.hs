module Rules.Dependencies (buildPackageDependencies) where

import Development.Shake.Util (parseMakefile)

import Base
import Context
import Expression
import Oracles.PackageData
import Rules.Actions
import Settings
import Target

-- TODO: simplify handling of AutoApply.cmm
buildPackageDependencies :: [(Resource, Int)] -> Context -> Rules ()
buildPackageDependencies rs context @ (Context {..}) =
    let path      = targetPath stage package
        buildPath = path -/- "build"
        dropBuild = (pkgPath package ++) . drop (length buildPath)
        hDepFile  = buildPath -/- ".hs-dependencies"
    in do
        fmap (buildPath ++)
            [ "//*.c.deps", "//*.cmm.deps", "//*.S.deps" ] |%> \out -> do
                let srcFile = if "//AutoApply.*" ?== out
                              then dropExtension out
                              else dropBuild . dropExtension $ out
                need [srcFile]
                build $ Target context (GccM stage) [srcFile] [out]

        hDepFile %> \out -> do
            srcs <- interpretInContext context getPackageSources
            need srcs
            if srcs == []
            then writeFileChanged out ""
            else buildWithResources rs $ Target context (GhcM stage) srcs [out]
            removeFileIfExists $ out <.> "bak"

        -- TODO: don't accumulate *.deps into .dependencies
        buildPath -/- ".dependencies" %> \out -> do
            cSrcs <- pkgDataList $ CSrcs path
            let cDepFiles = [ buildPath -/- src <.> "deps" | src <- cSrcs
                            , not ("//AutoApply.cmm" ?== src) ]
                         ++ [ src <.> "deps" | src <- cSrcs, "//AutoApply.cmm" ?== src ]

            need $ hDepFile : cDepFiles -- need all for more parallelism
            cDeps <- fmap concat $ traverse readFile' cDepFiles
            hDeps <- readFile' hDepFile
            let result = unlines
                       . map (\(src, deps) -> unwords $ src : deps)
                       . map (bimap unifyPath (map unifyPath))
                       . map (bimap head concat . unzip)
                       . groupBy ((==) `on` fst)
                       . sortBy (compare `on` fst)
                       . parseMakefile $ cDeps ++ hDeps
            writeFileChanged out result
