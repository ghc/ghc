{-# LANGUAGE NoImplicitPrelude #-}
module Rules.Data (
    ghcArgs, gccArgs, buildPackageDependencies
    ) where

import qualified Ways
import Base hiding (arg, args, Args)
import Package
import Expression.Base
import Oracles.Flag (when)
import Oracles.Builder
import Targets
import Switches
import Util

packageSettings :: Settings
packageSettings = msum
    [ args ["-hide-all-packages", "-no-user-package-db", "-include-pkg-deps"]
    , stage Stage0 ?
      (arg "-package-db" |> argPath "libraries/bootstrapping.conf")
    , supportsPackageKey ? notStage Stage0 ??
      ( argPairs "-this-package-key" argPackageKey <|>
        argPairs "-package-key"      argPackageDepKeys
      , argPairs "-package-name"     argPackageKey <|>
        argPairs "-package"          argPackageDeps )]

ghcArgs :: Settings
ghcArgs =
    let pathDist = path </> dist
        buildDir = unifyPath $ pathDist </> "build"
        depFile  = buildDir </> "haskell.deps"
    in msum [ arg "-M"
            , packageSettings
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


-- Build package-data.mk by using GhcCabal to process pkgCabal file
buildPackageData :: Stage -> Package -> FilePath -> Ways -> Settings -> Rules ()
buildPackageData stage pkg dir ways settings =
    (dir </>) <$>
    [ "package-data.mk"
    , "haddock-prologue.txt"
    , "inplace-pkg-config"
    , "setup-config"
    , "build" </> "autogen" </> "cabal_macros.h"
    -- TODO: Is this needed? Also check out Paths_cpsa.hs.
    -- , "build" </> "autogen" </> ("Paths_" ++ name) <.> "hs"
    ] &%> \_ -> do
        let configure = pkgPath pkg </> "configure"
        need [pkgPath pkg </> pkgCabal pkg]
        -- GhcCabal will run the configure script, so we depend on it
        -- We still don't know who build the configure script from configure.ac
        when (doesFileExist $ configure <.> "ac") $ need [configure]
        run' GhcCabal settings
        -- TODO: when (registerPackage settings) $
        run' (GhcPkg stage) settings
        postProcessPackageData $ dir </> "package-data.mk"

run' :: Builder -> Settings -> Action ()
run' builder settings = do
    settings' <- evaluate (project builder settings)
    case fromSettings settings' of
        Nothing   ->
            redError $ "Cannot determine " ++ show builder ++ " settings."
        Just args -> do
            putColoured Green (show args)
            run builder args

--buildRule :: Package -> TodoItem -> Rules ()
--buildRule pkg @ (Package name path cabal _) todo @ (stage, dist, settings) =
--    let pathDist  = path </> dist
--        cabalPath = path </> cabal
--        configure = path </> "configure"
--    in
--    -- All these files are produced by a single run of GhcCabal
--    (pathDist </>) <$>
--    [ "package-data.mk"
--    , "haddock-prologue.txt"
--    , "inplace-pkg-config"
--    , "setup-config"
--    , "build" </> "autogen" </> "cabal_macros.h"
--    -- TODO: Is this needed? Also check out Paths_cpsa.hs.
--    -- , "build" </> "autogen" </> ("Paths_" ++ name) <.> "hs"
--    ] &%> \_ -> do
--        need [cabalPath]
--        when (doesFileExist $ configure <.> "ac") $ need [configure]
--        -- GhcCabal will run the configure script, so we depend on it
--        -- We still don't know who build the configure script from configure.ac
--        run GhcCabal $ cabalArgs pkg todo
--        when (registerPackage settings) $
--            run (GhcPkg stage) $ ghcPkgArgs pkg todo
--        postProcessPackageData $ pathDist </> "package-data.mk"

ccSettings :: Settings
ccSettings = msum
    [ package integerLibrary ? argPath "-Ilibraries/integer-gmp2/gmp"
    , builder GhcCabal ? argStagedConfig "conf-cc-args"
    , validating ? msum
        [ not (builder GhcCabal) ? arg "-Werror"
        , arg "-Wall"
        , gccIsClang ??
          ( arg "-Wno-unknown-pragmas" <|>
            not gccLt46 ? windowsHost ? arg "-Werror=unused-but-set-variable"
          , not gccLt46 ? arg "-Wno-error=inline" )]]

ldSettings :: Settings
ldSettings = builder GhcCabal ? argStagedConfig "conf-gcc-linker-args"

cppSettings :: Settings
cppSettings = builder GhcCabal ? argStagedConfig "conf-cpp-args"
