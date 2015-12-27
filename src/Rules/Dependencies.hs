module Rules.Dependencies (buildPackageDependencies) where

import Expression
import GHC
import Oracles
import Rules.Actions
import Rules.Generate
import Rules.Resources
import Settings

buildPackageDependencies :: Resources -> PartialTarget -> Rules ()
buildPackageDependencies _ target @ (PartialTarget stage pkg) =
    let path      = targetPath stage pkg
        buildPath = path -/- "build"
        dropBuild = (pkgPath pkg ++) . drop (length buildPath)
        hDepFile  = buildPath -/- ".hs-dependencies"
        platformH = targetPath stage compiler -/- "ghc_boot_platform.h"
    in do
        (buildPath <//> "*.c.deps") %> \out -> do
            let srcFile = dropBuild . dropExtension $ out
            when (pkg == compiler) . need $ platformH : includesDependencies
            need [srcFile]
            build $ fullTarget target (GccM stage) [srcFile] [out]

        hDepFile %> \out -> do
            srcs <- interpretPartial target getPackageSources
            when (pkg == compiler) . need $ platformH : includesDependencies
            -- TODO: very ugly and fragile; use gcc -MM instead?
            let extraDeps = if pkg /= compiler then [] else fmap (buildPath -/-)
                   [ "primop-vector-uniques.hs-incl"
                   , "primop-data-decl.hs-incl"
                   , "primop-tag.hs-incl"
                   , "primop-list.hs-incl"
                   , "primop-strictness.hs-incl"
                   , "primop-fixity.hs-incl"
                   , "primop-primop-info.hs-incl"
                   , "primop-out-of-line.hs-incl"
                   , "primop-has-side-effects.hs-incl"
                   , "primop-can-fail.hs-incl"
                   , "primop-code-size.hs-incl"
                   , "primop-commutable.hs-incl"
                   , "primop-vector-tys-exports.hs-incl"
                   , "primop-vector-tycons.hs-incl"
                   , "primop-vector-tys.hs-incl" ]
            need $ srcs ++ extraDeps
            if srcs == []
            then writeFileChanged out ""
            else build $ fullTarget target (GhcM stage) srcs [out]
            removeFileIfExists $ out <.> "bak"

        (buildPath -/- ".dependencies") %> \out -> do
            cSrcs <- pkgDataList $ CSrcs path
            let cDepFiles = [ buildPath -/- src <.> "deps" | src <- cSrcs ]
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
