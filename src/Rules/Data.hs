module Rules.Data (buildPackageData) where

import Base
import Context
import Expression
import GHC
import Oracles.Dependencies
import Oracles.Setting
import Rules.Generate
import Settings.Path
import Target
import UserSettings
import Util

-- | Build @package-data.mk@ by using ghc-cabal utility to process .cabal files.
buildPackageData :: Context -> Rules ()
buildPackageData context@Context {..} = do
    let path      = buildPath context
        cabalFile = pkgCabalFile package
        configure = pkgPath package -/- "configure"
        dataFile  = pkgDataFile context
        setupConfigFile = pkgSetupConfigFile context

    [dataFile, setupConfigFile] &%> \(mk:setupConfig:_) -> do
        -- Make sure all generated dependencies are in place before proceeding.
        orderOnly =<< interpretInContext context generatedDependencies

        -- GhcCabal may run the configure script, so we depend on it.
        whenM (doesFileExist $ configure <.> "ac") $ need [configure]

        -- Before we configure a package its dependencies need to be registered.
        need =<< mapM pkgConfFile =<< contextDependencies context

        need [cabalFile]
        build $ target context GhcCabal [cabalFile] [mk, setupConfig]
        postProcessPackageData context mk

    pkgInplaceConfig context %> \conf -> do
        need [dataFile] -- ghc-cabal builds inplace package configuration file
        if package == rts
        then do
            need [rtsConfIn]
            build $ target context HsCpp [rtsConfIn] [conf]
            fixFile conf $ unlines
                         . map
                         ( replace "\"\"" ""
                         . replace "rts/dist/build" rtsBuildPath
                         . replace "includes/dist-derivedconstants/header" generatedPath )
                         . lines
        else
            fixFile conf $ unlines . map (replace (path </> "build") path) . lines

    priority 2.0 $ when (nonCabalContext context) $ dataFile %>
        generatePackageData context

generatePackageData :: Context -> FilePath -> Action ()
generatePackageData context@Context {..} file = do
    orderOnly =<< interpretInContext context generatedDependencies
    asmSrcs <- packageAsmSources package
    cSrcs   <- packageCSources   package
    cmmSrcs <- packageCmmSources package
    let pkgKey = if isLibrary package then "COMPONENT_ID = " else "PROGNAME = "
    writeFileChanged file . unlines $
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
-- 1) Drop lines containing '$'. For example, get rid of
-- @libraries/Win32_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir ...@
-- and replace it with a tracked call to getDirectoryFiles.
-- 2) Drop path prefixes to individual settings.
-- For example, @libraries/deepseq/dist-install_VERSION = 1.4.0.0@
-- is replaced by @VERSION = 1.4.0.0@.
-- Reason: Shake's built-in makefile parser doesn't recognise slashes
-- TODO (izgzhen): should fix DEP_LIB_REL_DIRS_SEARCHPATH
postProcessPackageData :: Context -> FilePath -> Action ()
postProcessPackageData context@Context {..} file = do
    top     <- topDirectory
    cmmSrcs <- getDirectoryFiles (pkgPath package) ["cbits/*.cmm"]
    let len = length (pkgPath package) + length (top -/- buildPath context) + 2
    fixFile file $ unlines
                 . (++ ["CMM_SRCS = " ++ unwords (map unifyPath cmmSrcs) ])
                 . map (drop len) . filter ('$' `notElem`) . lines
