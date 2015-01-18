{-# LANGUAGE NoImplicitPrelude #-}
module Package.Dependencies (buildPackageDependencies) where

import Package.Base

argListDir :: FilePath
argListDir = "shake/arg/buildPackageDependencies"

ghcArgs :: Package -> TodoItem -> Args
ghcArgs (Package name path _) (stage, dist, settings) =
    let pathDist = path </> dist
        buildDir = unifyPath $ pathDist </> "build"
        depFile  = buildDir </> "haskell.deps"
    in args [ arg "-M"
            , packageArgs stage pathDist
            , includeGhcArgs path dist
            , concatArgs ["-optP"] $ CppArgs pathDist
            , productArgs ["-odir", "-stubdir", "-hidir"] buildDir
            , args ["-dep-makefile", depFile ]
            , productArgs "-dep-suffix" $ map wayPrefix <$> ways settings
            , args $ HsArgs pathDist
            , args $ pkgHsSources path dist ]

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
gccArgs :: FilePath -> Package -> TodoItem -> Args
gccArgs sourceFile (Package _ path _) (stage, dist, _) =
    let pathDist = path </> dist
        buildDir = pathDist </> "build"
        depFile  = buildDir </> takeFileName sourceFile <.> "deps"
    in args [ arg "-MM"
            , args $ CcArgs pathDist
            , commonCcArgs
            , commonCcWarninigArgs
            , pathArgs "-I" path $ IncludeDirs pathDist
            , args ["-MF", unifyPath depFile]
            , args ["-x", "c"]
            , arg $ unifyPath sourceFile ]

buildRule :: Package -> TodoItem -> Rules ()
buildRule pkg @ (Package name path _) todo @ (stage, dist, settings) = do
    let pathDist = path </> dist
        buildDir = pathDist </> "build"

    (buildDir </> "haskell.deps") %> \out -> do
        need [argListPath argListDir pkg stage]
        run (Ghc stage) $ ghcArgs pkg todo

    (buildDir </> "c.deps") %> \out -> do
        need [argListPath argListDir pkg stage]
        srcs <- args $ CSrcs pathDist
        deps <- fmap concat $ forM srcs $ \src -> do
            let srcPath = path </> src
                depFile = buildDir </> takeFileName src <.> "deps"
            run (Gcc stage) $ gccArgs srcPath pkg todo
            liftIO $ readFile depFile
        writeFileChanged out deps
        liftIO $ removeFiles buildDir ["*.c.deps"]

argListRule :: Package -> TodoItem -> Rules ()
argListRule pkg todo @ (stage, _, _) =
    (argListPath argListDir pkg stage) %> \out -> do
        need $ ["shake/src/Package/Dependencies.hs"] ++ sourceDependecies
        ghcList <- argList (Ghc stage) $ ghcArgs pkg todo
        gccList <- argList (Gcc stage) $ gccArgs "source.c" pkg todo
        writeFileChanged out $ ghcList ++ "\n" ++ gccList

buildPackageDependencies :: Package -> TodoItem -> Rules ()
buildPackageDependencies = argListRule <> buildRule
