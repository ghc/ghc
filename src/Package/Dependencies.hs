{-# LANGUAGE NoImplicitPrelude #-}
module Package.Dependencies (buildPackageDependencies) where

import Package.Base

-- TODO: use oracles instead of arg files.
argListDir :: FilePath
argListDir = "shake/arg/buildPackageDependencies"

ghcArgs :: Package -> TodoItem -> Args
ghcArgs (Package name path _ _) (stage, dist, settings) =
    let pathDist = path </> dist
        buildDir = unifyPath $ pathDist </> "build"
        depFile  = buildDir </> "haskell.deps"
    in args [ arg "-M"
            , packageArgs stage pathDist
            , includeGhcArgs path dist
            , concatArgs ["-optP"] $ CppArgs pathDist
            , productArgs ["-odir", "-stubdir", "-hidir"] [buildDir]
            , args ["-dep-makefile", depFile]
            , productArgs ["-dep-suffix"] $ map wayPrefix <$> ways settings
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
gccArgs sourceFile (Package _ path _ _) (stage, dist, settings) =
    let pathDist = path </> dist
        buildDir = pathDist </> "build"
        depFile  = buildDir </> takeFileName sourceFile <.> "deps"
    in args [ args ["-E", "-MM"] -- TODO: add a Cpp Builder instead
            , args $ CcArgs pathDist
            , commonCcArgs          -- TODO: remove?
            , customCcArgs settings -- TODO: Replace by customCppArgs?
            , commonCcWarninigArgs  -- TODO: remove?
            , includeGccArgs path dist
            , args ["-MF", unifyPath depFile]
            , args ["-x", "c"]
            , arg $ unifyPath sourceFile ]

buildRule :: Package -> TodoItem -> Rules ()
buildRule pkg @ (Package name path _ _) todo @ (stage, dist, settings) = do
    let pathDist = path </> dist
        buildDir = pathDist </> "build"

    (buildDir </> "haskell.deps") %> \_ -> do
        run (Ghc stage) $ ghcArgs pkg todo
        -- Finally, record the argument list
        need [argListPath argListDir pkg stage]

    (buildDir </> "c.deps") %> \out -> do
        srcs <- args $ CSrcs pathDist
        deps <- fmap concat $ forM srcs $ \src -> do
            let srcPath = path </> src
                depFile = buildDir </> takeFileName src <.> "deps"
            run (Gcc stage) $ gccArgs srcPath pkg todo
            liftIO $ readFile depFile
        writeFileChanged out deps
        liftIO $ removeFiles buildDir ["*.c.deps"]
        -- Finally, record the argument list
        need [argListPath argListDir pkg stage]

argListRule :: Package -> TodoItem -> Rules ()
argListRule pkg todo @ (stage, _, _) =
    (argListPath argListDir pkg stage) %> \out -> do
        need $ ["shake/src/Package/Dependencies.hs"] ++ sourceDependecies
        ghcList <- argList (Ghc stage) $ ghcArgs pkg todo
        gccList <- argList (Gcc stage) $ gccArgs "source.c" pkg todo
        writeFileChanged out $ ghcList ++ "\n" ++ gccList

buildPackageDependencies :: Package -> TodoItem -> Rules ()
buildPackageDependencies = argListRule <> buildRule
