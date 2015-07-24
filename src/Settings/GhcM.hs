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
    stage <- asks getStage
    builder (GhcM stage) ? do
        pkg     <- asks getPackage
        cppArgs <- getPkgDataList CppArgs
        hsArgs  <- getPkgDataList HsArgs
        hsSrcs  <- getHsSources
        ways    <- fromDiffExpr Settings.Ways.ways
        let buildPath = targetPath stage pkg -/- "build"
        mconcat
            [ arg "-M"
            , packageGhcArgs
            , includeGhcArgs
            , append . map ("-optP" ++) $ cppArgs
            , arg $ "-odir " ++ buildPath
            , arg $ "-stubdir " ++ buildPath
            , arg $ "-hidir " ++ buildPath
            , arg $ "-dep-makefile " ++ buildPath -/- "haskell.deps"
            , append . map (\way -> "-dep-suffix " ++ wayPrefix way) $ ways
            , append hsArgs
            , append hsSrcs ]

packageGhcArgs :: Args
packageGhcArgs = do
    stage              <- asks getStage
    supportsPackageKey <- lift . flag $ SupportsPackageKey
    pkgKey             <- getPkgData     PackageKey
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
    stage       <- asks getStage
    pkg         <- asks getPackage
    srcDirs     <- getPkgDataList SrcDirs
    includeDirs <- getPkgDataList IncludeDirs
    let buildPath   = targetPath stage pkg -/- "build"
        autogenPath = buildPath -/- "autogen"
    mconcat
        [ arg "-i"
        , append . map (\dir -> "-i" ++ pkgPath pkg -/- dir) $ srcDirs
        , arg $ "-i" ++ buildPath
        , arg $ "-i" ++ autogenPath
        , arg $ "-I" ++ buildPath
        , arg $ "-I" ++ autogenPath
        , append . map (\dir -> "-I" ++ pkgPath pkg -/- dir) $ includeDirs
        , arg "-optP-include" -- TODO: Shall we also add -cpp?
        , arg $ "-optP" ++ autogenPath -/- "cabal_macros.h" ]

getHsSources :: Expr [FilePath]
getHsSources = do
    stage   <- asks getStage
    pkg     <- asks getPackage
    srcDirs <- getPkgDataList SrcDirs
    let autogenPath = targetPath stage pkg -/- "build/autogen"
        dirs        = autogenPath : map (pkgPath pkg -/-) srcDirs
    getModuleFiles dirs [".hs", ".lhs"]

getModuleFiles :: [FilePath] -> [String] -> Expr [FilePath]
getModuleFiles directories suffixes = do
    modules <- getPkgDataList Modules
    let modPaths = map (replaceEq '.' pathSeparator) modules
    files <- lift $ forM [ dir -/- modPath ++ suffix
                         | dir     <- directories
                         , modPath <- modPaths
                         , suffix  <- suffixes
                         ] $ \file -> do
                             let dir = takeDirectory file
                             dirExists <- doesDirectoryExist dir
                             return [ unifyPath file | dirExists ]
    result <- lift $ getDirectoryFiles "" (concat files)
    return $ map unifyPath result


-- $1_$2_$3_ALL_CC_OPTS = \
-- $$(WAY_$3_CC_OPTS) \
-- $$($1_$2_DIST_GCC_CC_OPTS) \
-- $$($1_$2_$3_CC_OPTS) \
-- $$($$(basename $$<)_CC_OPTS) \
-- $$($1_$2_EXTRA_CC_OPTS) \
-- $$(EXTRA_CC_OPTS)
--
-- $1_$2_DIST_CC_OPTS = \
-- $$(SRC_CC_OPTS) \
-- $$($1_CC_OPTS) \
-- -I$1/$2/build/autogen \
-- $$(foreach dir,$$(filter-out /%,$$($1_$2_INCLUDE_DIRS)),-I$1/$$(dir)) \
-- $$(foreach dir,$$(filter /%,$$($1_$2_INCLUDE_DIRS)),-I$$(dir)) \
-- $$($1_$2_CC_OPTS) \
-- $$($1_$2_CPP_OPTS) \
-- $$($1_$2_CC_INC_FLAGS) \
-- $$($1_$2_DEP_CC_OPTS) \
-- $$(SRC_CC_WARNING_OPTS)

-- TODO: handle custom $1_$2_MKDEPENDC_OPTS and
-- gccArgs :: FilePath -> Package -> TodoItem -> Args
-- gccArgs sourceFile (Package _ path _ _) (stage, dist, settings) =
--     let pathDist = path </> dist
--         buildDir = pathDist </> "build"
--         depFile  = buildDir </> takeFileName sourceFile <.> "deps"
--     in args [ args ["-E", "-MM"] -- TODO: add a Cpp Builder instead
--             , args $ CcArgs pathDist
--             , commonCcArgs          -- TODO: remove?
--             , customCcArgs settings -- TODO: Replace by customCppArgs?
--             , commonCcWarninigArgs  -- TODO: remove?
--             , includeGccArgs path dist
--             , args ["-MF", unifyPath depFile]
--             , args ["-x", "c"]
--             , arg $ unifyPath sourceFile ]

-- buildRule :: Package -> TodoItem -> Rules ()
-- buildRule pkg @ (Package name path _ _) todo @ (stage, dist, settings) = do
--     let pathDist = path </> dist
--         buildDir = pathDist </> "build"

--     (buildDir </> "haskell.deps") %> \_ -> do
--         run (Ghc stage) $ ghcArgs pkg todo
--         -- Finally, record the argument list
--         need [argListPath argListDir pkg stage]

--     (buildDir </> "c.deps") %> \out -> do
--         srcs <- args $ CSrcs pathDist
--         deps <- fmap concat $ forM srcs $ \src -> do
--             let srcPath = path </> src
--                 depFile = buildDir </> takeFileName src <.> "deps"
--             run (Gcc stage) $ gccArgs srcPath pkg todo
--             liftIO $ readFile depFile
--         writeFileChanged out deps
--         liftIO $ removeFiles buildDir ["*.c.deps"]
--         -- Finally, record the argument list
--         need [argListPath argListDir pkg stage]
