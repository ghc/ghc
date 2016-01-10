module Rules.Dependencies (buildPackageDependencies) where

import Base
import Expression
import Oracles
import Rules.Actions
import Rules.Resources
import Settings
import Development.Shake.Util (parseMakefile)

-- TODO: simplify handling of AutoApply.cmm
buildPackageDependencies :: Resources -> PartialTarget -> Rules ()
buildPackageDependencies _ target @ (PartialTarget stage pkg) =
    let path      = targetPath stage pkg
        buildPath = path -/- "build"
        dropBuild = (pkgPath pkg ++) . drop (length buildPath)
        hDepFile  = buildPath -/- ".hs-dependencies"
    in do
        fmap (buildPath++)
            [ "//*.c.deps", "//*.cmm.deps", "//*.S.deps" ] |%> \out -> do
                let srcFile = if "//AutoApply.*" ?== out
                              then dropExtension out
                              else dropBuild . dropExtension $ out
                need [srcFile]
                build $ fullTarget target (GccM stage) [srcFile] [out]

        hDepFile %> \out -> do
            srcs <- interpretPartial target getPackageSources
            need srcs
            if srcs == []
            then writeFileChanged out ""
            else build $ fullTarget target (GhcM stage) srcs [out]
            removeFileIfExists $ out <.> "bak"

        -- TODO: don't accumulate *.deps into .dependencies
        (buildPath -/- ".dependencies") %> \out -> do
            cSrcs <- pkgDataList $ CSrcs path
            let cDepFiles = [ buildPath -/- src <.> "deps" | src <- cSrcs
                            , not ("//AutoApply.cmm" ?== src) ]
                         ++ [ src <.> "deps" | src <- cSrcs, "//AutoApply.cmm" ?== src ]

            need $ hDepFile : cDepFiles -- need all for more parallelism
            cDeps <- fmap concat $ mapM readFile' cDepFiles
            hDeps <- readFile' hDepFile
            let result = unlines
                       . map (\(src, deps) -> unwords $ src : deps)
                       . map (bimap unifyPath (map unifyPath))
                       . map (bimap head concat . unzip)
                       . groupBy ((==) `on` fst)
                       . sortBy (compare `on` fst)
                       . parseMakefile $ cDeps ++ hDeps
            writeFileChanged out result
