{-# LANGUAGE NoImplicitPrelude #-}
module Rules.Data (
    cabalSettings, ghcPkgSettings, buildPackageData
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

librarySettings :: Ways -> Settings
librarySettings ways = msum
    [ whenExists Ways.vanilla ways     ?? ( arg  "--enable-library-vanilla"
                                          , arg "--disable-library-vanilla" )
    , (ghcWithInterpreter
      && not dynamicGhcPrograms
      && whenExists Ways.vanilla ways) ?? ( arg  "--enable-library-for-ghci"
                                          , arg "--disable-library-for-ghci" )
    , whenExists Ways.profiling ways   ?? ( arg  "--enable-library-profiling"
                                          , arg "--disable-library-profiling" )
    , whenExists Ways.dynamic ways     ?? ( arg  "--enable-shared"
                                          , arg "--disable-shared" )]

configureSettings :: Settings
configureSettings =
    let conf key = argPrefix ("--configure-option=" ++ key ++ "=")
                 . argConcatSpace
    in
    msum [ conf "CFLAGS"   ccSettings
         , conf "LDFLAGS"  ldSettings
         , conf "CPPFLAGS" cppSettings
         , argPrefix "--gcc-options=" $
           argConcatSpace (ccSettings <|> ldSettings)
         , conf "--with-iconv-includes"  (argConfig "iconv-include-dirs")
         , conf "--with-iconv-libraries" (argConfig "iconv-lib-dirs")
         , conf "--with-gmp-includes"    (argConfig "gmp-include-dirs")
         , conf "--with-gmp-libraries"   (argConfig "gmp-lib-dirs")
         -- TODO: why TargetPlatformFull and not host?
         , crossCompiling ?
           conf "--host"    (argConfig "target-platform-full")
         , conf "--with-cc" (argStagedBuilderPath Gcc) ]

bootPackageDbSettings :: Settings
bootPackageDbSettings =
    stage Stage0 ?
        argPrefix "--package-db="
        (argConcatPath $ argConfig "ghc-source-path" |>
                         argPath "libraries/bootstrapping.conf")

dllSettings :: Settings
dllSettings = arg ""

cabalSettings :: Settings
cabalSettings =
    mproduct
    [ arg "configure" -- start with builder, e.g. argBuilderPath GhcCabal?
    , argBuildPath
    , argBuildDir
    , dllSettings
    , msum
      [ argWithStagedBuilder Ghc -- TODO: used to be limited to max stage1 GHC
      , argWithStagedBuilder GhcPkg
      , customConfigureSettings
      , stage Stage0 ? bootPackageDbSettings
      , librarySettings targetWays
      , configNonEmpty "hscolour" ? argWithBuilder HsColour -- TODO: more reuse
      , configureSettings
      , argPackageConstraints (stage Stage0 ? targetPackages)
      , argWithStagedBuilder Gcc
      , notStage Stage0 ? argWithBuilder Ld
      , argWithBuilder Ar
      , argWithBuilder Alex
      , argWithBuilder Happy ]] -- TODO: reorder with's

ghcPkgSettings :: Settings
ghcPkgSettings =
    arg "update" |> msum
        [ arg "--force"
        , argConcatPath $
          msum [argBuildPath, argBuildDir, arg "inplace-pkg-config"]
        , bootPackageDbSettings ]

-- Prepare a given 'packaga-data.mk' file for parsing by readConfigFile:
-- 1) Drop lines containing '$'
-- For example, get rid of
-- libraries/Win32_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir ...
-- Reason: we don't need them and we can't parse them.
-- 2) Replace '/' and '\' with '_' before '='
-- For example libraries/deepseq/dist-install_VERSION = 1.4.0.0
-- is replaced by libraries_deepseq_dist-install_VERSION = 1.4.0.0
-- Reason: Shake's built-in makefile parser doesn't recognise slashes

postProcessPackageData :: FilePath -> Action ()
postProcessPackageData file = do
    pkgData <- (filter ('$' `notElem`) . lines) <$> liftIO (readFile file)
    length pkgData `seq` writeFileLines file $ map processLine pkgData
      where
        processLine line = replaceSeparators '_' prefix ++ suffix
          where
            (prefix, suffix) = break (== '=') line

-- this is a positional argument, hence:
-- * if it is empty, we need to emit one empty string argument
-- * otherwise, we must collapse it into one space-separated string

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
