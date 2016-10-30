module Rules.Data (buildPackageData) where

import Base
import Context
import Expression
import GHC
import Oracles.Config.Setting
import Oracles.Dependencies
import Rules.Generate
import Rules.Libffi
import Settings.Path
import Target
import UserSettings
import Util

-- | Build @package-data.mk@ by using ghc-cabal utility to process .cabal files.
buildPackageData :: Context -> Rules ()
buildPackageData context@Context {..} = do
    let cabalFile = pkgCabalFile package
        configure = pkgPath package -/- "configure"
        dataFile  = pkgDataFile context
        oldPath   = pkgPath package -/- stageDirectory stage -- TODO: remove, #113
        inTreeMk  = oldPath -/- takeFileName dataFile -- TODO: remove, #113

    inTreeMk %> \mk -> do
        -- Make sure all generated dependencies are in place before proceeding.
        orderOnly =<< interpretInContext context generatedDependencies

        -- GhcCabal may run the configure script, so we depend on it.
        whenM (doesFileExist $ configure <.> "ac") $ need [configure]

        -- Before we configure a package its dependencies need to be registered.
        need =<< mapM pkgConfFile =<< contextDependencies context

        need [cabalFile]
        build $ Target context GhcCabal [cabalFile] [mk]

    -- TODO: Get rid of this, see #113.
    dataFile %> \mk -> do
        -- TODO: This is a hack. Add a proper support for autogen directory
        -- structure of the new Cabal (probably only after #113).
        let oldBuild
                | isLibrary package   = oldPath -/- "build"
                | package == ghc      = oldPath -/- "build/ghc"
                | package == hpcBin   = oldPath -/- "build/hpc"
                | package == iservBin = oldPath -/- "build/iserv"
                | otherwise           = oldPath -/- "build" -/- pkgNameString package
        copyFile inTreeMk mk
        autogenFiles <- getDirectoryFiles oldBuild ["autogen/*"]
        createDirectory $ buildPath context -/- "autogen"
        forM_ autogenFiles $ \file' -> do
            let file = unifyPath file'
            copyFile (oldBuild -/- file) (buildPath context -/- file)
        let haddockPrologue = "haddock-prologue.txt"
        copyFile (oldPath -/- haddockPrologue) (buildPath context -/- haddockPrologue)
        postProcessPackageData context mk

    -- TODO: PROGNAME was $(CrossCompilePrefix)hp2ps.
    priority 2.0 $ do
        when (package `elem` [hp2ps, rts, touchy, unlit]) $ dataFile %>
            generatePackageData context

        -- Bootstrapping `ghcCabal`: although `ghcCabal` is a proper cabal
        -- package, we cannot generate the corresponding `package-data.mk` file
        -- by running by running `ghcCabal`, because it has not yet been built.
        when (package == ghcCabal && stage == Stage0) $ dataFile %>
            generatePackageData context

generatePackageData :: Context -> FilePath -> Action ()
generatePackageData context@Context {..} file = do
    orderOnly =<< interpretInContext context generatedDependencies
    asmSrcs <- packageAsmSources package
    cSrcs   <- packageCSources   package
    cmmSrcs <- packageCmmSources package
    let prefix = fixKey (buildPath context) ++ "_"
        pkgKey = if isLibrary package then "COMPONENT_ID = " else "PROGNAME = "
    writeFileChanged file . unlines . map (prefix ++) $
        [ pkgKey ++ pkgNameString package                                   ] ++
        [ "S_SRCS = "   ++ unwords asmSrcs                                  ] ++
        [ "C_SRCS = "   ++ unwords cSrcs                                    ] ++
        [ "CMM_SRCS = " ++ unwords cmmSrcs                                  ] ++
        [ "DEP_EXTRA_LIBS = m"                 | package == hp2ps           ] ++
        [ "CC_OPTS = -I" ++ generatedPath      | package `elem` [hp2ps, rts]] ++
        [ "MODULES = Main"                     | package == ghcCabal        ] ++
        [ "HS_SRC_DIRS = ."                    | package == ghcCabal        ] ++
        [ "SYNOPSIS = Bootstrapped ghc-cabal." | package == ghcCabal        ]
    putSuccess $ "| Successfully generated " ++ file

packageCSources :: Package -> Action [FilePath]
packageCSources pkg
    | pkg /= rts = getDirectoryFiles (pkgPath pkg) ["*.c"]
    | otherwise  = do
        windows <- windowsHost
        sources <- fmap (map unifyPath) . getDirectoryFiles (pkgPath pkg) .
            map (-/- "*.c") $ [ ".", "hooks", "sm", "eventlog", "linker" ] ++
                              [ if windows then "win32" else "posix"     ]
        return $ sources ++ [ rtsBuildPath -/- "c/sm/Evac_thr.c" ]
                         ++ [ rtsBuildPath -/- "c/sm/Scav_thr.c" ]

packageAsmSources :: Package -> Action [FilePath]
packageAsmSources pkg
    | pkg /= rts = return []
    | otherwise  = do
        buildAdjustor   <- anyTargetArch ["i386", "powerpc", "powerpc64"]
        buildStgCRunAsm <- anyTargetArch ["powerpc64le"]
        return $ [ "AdjustorAsm.S" | buildAdjustor   ]
              ++ [ "StgCRunAsm.S"  | buildStgCRunAsm ]

packageCmmSources :: Package -> Action [FilePath]
packageCmmSources pkg
    | pkg /= rts = return []
    | otherwise  = do
        sources <- getDirectoryFiles (pkgPath pkg) ["*.cmm"]
        return $ sources ++ [ rtsBuildPath -/- "cmm/AutoApply.cmm" ]

-- Prepare a given 'packaga-data.mk' file for parsing by readConfigFile:
-- 1) Drop lines containing '$'
-- For example, get rid of
-- libraries/Win32_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir ...
-- Reason: we don't need them and we can't parse them.
-- 2) Replace '/' and '\' with '_' before '='
-- For example libraries/deepseq/dist-install_VERSION = 1.4.0.0
-- is replaced by libraries_deepseq_dist-install_VERSION = 1.4.0.0
-- Reason: Shake's built-in makefile parser doesn't recognise slashes
postProcessPackageData :: Context -> FilePath -> Action ()
postProcessPackageData Context {..} file = fixFile file fixPackageData
  where
    fixPackageData = unlines . map processLine . filter (not . null) . filter ('$' `notElem`) . lines
    processLine line = fixKey fixedPrefix ++ suffix
      where
        (prefix, suffix) = break (== '=') line
        -- Change package/path/targetDir to takeDirectory file
        -- This is a temporary hack until we get rid of ghc-cabal
        fixedPrefix = takeDirectory file ++ drop len prefix
        len         = length (pkgPath package -/- stageDirectory stage)

-- TODO: Remove, see #113.
fixKey :: String -> String
fixKey = replaceSeparators '_'
