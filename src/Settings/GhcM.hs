module Settings.GhcM (ghcMArgs) where

import Way
import Util
import Stage
import Builder
import Package
import Switches
import Expression
import Oracles.Flag
import Oracles.PackageData
import Settings.Util
import Settings.Ways
import Settings.TargetDirectory
import Development.Shake

ghcMArgs :: Args
ghcMArgs = do
    stage <- getStage
    builder (GhcM stage) ? do
        pkg     <- getPackage
        cppArgs <- getPkgDataList CppArgs
        hsArgs  <- getPkgDataList HsArgs
        hsSrcs  <- getHsSources
        ways    <- getWays
        let buildPath = targetPath stage pkg -/- "build"
        mconcat
            [ arg "-M"
            , packageGhcArgs
            , includeGhcArgs
            , append . map ("-optP" ++) $ cppArgs
            , arg "-odir"        , arg buildPath
            , arg "-stubdir"     , arg buildPath
            , arg "-hidir"       , arg buildPath
            , arg "-dep-makefile", arg $ buildPath -/- "haskell.deps"
            , append . concatMap (\way -> ["-dep-suffix", wayPrefix way]) $ ways
            , append hsArgs
            , append hsSrcs ]

packageGhcArgs :: Args
packageGhcArgs = do
    stage              <- getStage
    supportsPackageKey <- getFlag SupportsPackageKey
    pkgKey             <- getPkgData PackageKey
    pkgDepKeys         <- getPkgDataList DepKeys
    pkgDeps            <- getPkgDataList Deps
    mconcat
        [ arg "-hide-all-packages"
        , arg "-no-user-package-db"
        , arg "-include-pkg-deps"
        , stage0 ? arg "-package-db libraries/bootstrapping.conf"
        , if supportsPackageKey || stage /= Stage0
          then mconcat [ arg $ "-this-package-key " ++ pkgKey
                       , append . map ("-package-key " ++) $ pkgDepKeys ]
          else mconcat [ arg $ "-package-name" ++ pkgKey
                       , append . map ("-package " ++) $ pkgDeps ]]

includeGhcArgs :: Args
includeGhcArgs = do
    stage   <- getStage
    pkg     <- getPackage
    srcDirs <- getPkgDataList SrcDirs
    incDirs <- getPkgDataList IncludeDirs
    let buildPath   = targetPath stage pkg -/- "build"
        autogenPath = buildPath -/- "autogen"
    mconcat
        [ arg "-i"
        , append . map (\dir -> "-i" ++ pkgPath pkg -/- dir) $ srcDirs
        , arg $ "-i" ++ buildPath
        , arg $ "-i" ++ autogenPath
        , arg $ "-I" ++ buildPath
        , arg $ "-I" ++ autogenPath
        , append . map (\dir -> "-I" ++ pkgPath pkg -/- dir) $ incDirs
        , arg "-optP-include" -- TODO: Shall we also add -cpp?
        , arg $ "-optP" ++ autogenPath -/- "cabal_macros.h" ]

getHsSources :: Expr [FilePath]
getHsSources = do
    stage   <- getStage
    pkg     <- getPackage
    srcDirs <- getPkgDataList SrcDirs
    let autogen = targetPath stage pkg -/- "build/autogen"
        paths   = autogen : map (pkgPath pkg -/-) srcDirs
    getSourceFiles paths [".hs", ".lhs"]

-- Find all source files in specified paths and with given extensions
getSourceFiles :: [FilePath] -> [String] -> Expr [FilePath]
getSourceFiles paths exts = do
    modules <- getPkgDataList Modules
    let modPaths   = map (replaceEq '.' '/') modules
        candidates = [ p -/- m ++ e | p <- paths, m <- modPaths, e <- exts ]
    files <- lift $ filterM (doesDirectoryExist . takeDirectory) candidates
    result <- lift $ getDirectoryFiles "" files
    return $ map unifyPath result
