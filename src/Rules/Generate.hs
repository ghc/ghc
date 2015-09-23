module Rules.Generate (generatePackageCode) where

import Expression
import Oracles
import Rules.Actions
import Rules.Resources
import Settings

primops :: FilePath
primops = "compiler/stage1/build/primops.txt"

-- The following generators and corresponding source extensions are supported:
knownGenerators :: [ (Builder, String) ]
knownGenerators =  [ (Alex   , ".x"  )
                   , (Happy  , ".y"  )
                   , (Happy  , ".ly" )
                   , (Hsc2Hs , ".hsc") ]

determineBuilder :: FilePath -> Maybe Builder
determineBuilder file = fmap fst $ find (\(_, e) -> e == ext) knownGenerators
  where
    ext = takeExtension file

generatePackageCode :: Resources -> PartialTarget -> Rules ()
generatePackageCode _ target @ (PartialTarget stage package) =
    let path        = targetPath stage package
        packagePath = pkgPath package
        buildPath   = path -/- "build"
    in do -- TODO: do we need to copy *.(l)hs-boot files here? Never happens?
        buildPath </> "*.hs" %> \file -> do
            dirs  <- interpretPartial target $ getPkgDataList SrcDirs
            files <- getDirectoryFiles "" $
                [ packagePath </> d </> takeBaseName file <.> "*" | d <- dirs ]
            let gens = [ (f, b) | f <- files, Just b <- [determineBuilder f] ]
            when (length gens /= 1) . putError $
                "Exactly one generator expected for " ++ file
                ++ " (found: " ++ show gens ++ ")."
            let (src, builder) = head gens
            need [src]
            build $ fullTarget target builder [src] [file]

        path -/- "build/GHC/PrimopWrappers.hs" %> \file -> do
            need [primops]
            build $ fullTarget target GenPrimopCode [primops] [file]

        priority 2.0 $ path -/- "build/Config.hs" %> \file -> do
            config <- generateConfig
            writeFileChanged file config

generateConfig :: Action String
generateConfig = do
    cProjectName        <- setting ProjectName
    cProjectGitCommitId <- setting ProjectGitCommitId
    cProjectVersion     <- setting ProjectVersion
    cProjectVersionInt  <- setting ProjectVersionInt
    cProjectPatchLevel  <- setting ProjectPatchLevel
    cProjectPatchLevel1 <- setting ProjectPatchLevel1
    cProjectPatchLevel2 <- setting ProjectPatchLevel2
    cBooterVersion      <- setting GhcVersion
    cIntegerLibraryType <- case integerLibrary of
        integerGmp    -> return "IntegerGMP"
        integerSimple -> return "IntegerSimple"
        _ -> putError $ "Unknown integer library: " ++ integerLibrary ++ "."
    cSupportsSplitObjs  <- yesNo splitObjects
    return "{-# LANGUAGE CPP #-}\n"
        ++ "module Config where\n"
        ++ "\n"
        ++ "#include \"ghc_boot_platform.h\"\n"
        ++ "\n"
        ++ "data IntegerLibrary = IntegerGMP\n"
        ++ "                    | IntegerSimple\n"
        ++ "                    deriving Eq\n"
        ++ "\n"
        ++ "cBuildPlatformString :: String\n"
        ++ "cBuildPlatformString = BuildPlatform_NAME\n"
        ++ "cHostPlatformString :: String\n"
        ++ "cHostPlatformString = HostPlatform_NAME\n"
        ++ "cTargetPlatformString :: String\n"
        ++ "cTargetPlatformString = TargetPlatform_NAME\n"
        ++ "\n"
        ++ "cProjectName          :: String\n"
        ++ "cProjectName          = " ++ cProjectName ++ "\n"
        ++ "cProjectGitCommitId   :: String\n"
        ++ "cProjectGitCommitId   = " ++ cProjectGitCommitId ++ "\n"
        ++ "cProjectVersion       :: String\n"
        ++ "cProjectVersion       = " ++ cProjectVersion ++ "\n"
        ++ "cProjectVersionInt    :: String\n"
        ++ "cProjectVersionInt    = " ++ cProjectVersionInt ++ "\n"
        ++ "cProjectPatchLevel    :: String\n"
        ++ "cProjectPatchLevel    = " ++ cProjectPatchLevel ++ "\n"
        ++ "cProjectPatchLevel1   :: String\n"
        ++ "cProjectPatchLevel1   = " ++ cProjectPatchLevel1 ++ "\n"
        ++ "cProjectPatchLevel2   :: String\n"
        ++ "cProjectPatchLevel2   = " ++ cProjectPatchLevel2 ++ "\n"
        ++ "cBooterVersion        :: String\n"
        ++ "cBooterVersion        = " ++ cBooterVersion ++ "\n"
        ++ "cStage                :: String\n"
        ++ "cStage                = show (STAGE :: Int)\n"
        ++ "cIntegerLibrary       :: String\n"
        ++ "cIntegerLibrary       = " ++ pkgName integerLibrary ++ "\n"
        ++ "cIntegerLibraryType   :: IntegerLibrary\n"
        ++ "cIntegerLibraryType   = " ++ cIntegerLibraryType ++ "\n"
        ++ "cSupportsSplitObjs    :: String\n"
        ++ "cSupportsSplitObjs    = " ++ cSupportsSplitObjs ++ "\n"
        ++ "cGhcWithInterpreter   :: String\n"
        ++ "cGhcWithInterpreter   = "YES"\n"
        ++ "cGhcWithNativeCodeGen :: String\n"
        ++ "cGhcWithNativeCodeGen = "YES"\n"
        ++ "cGhcWithSMP           :: String\n"
        ++ "cGhcWithSMP           = "YES"\n"
        ++ "cGhcRTSWays           :: String\n"
        ++ "cGhcRTSWays           = "l debug thr thr_debug thr_l thr_p "\n"
        ++ "cGhcEnableTablesNextToCode :: String\n"
        ++ "cGhcEnableTablesNextToCode = "YES"\n"
        ++ "cLeadingUnderscore    :: String\n"
        ++ "cLeadingUnderscore    = "NO"\n"
        ++ "cGHC_UNLIT_PGM        :: String\n"
        ++ "cGHC_UNLIT_PGM        = "unlit.exe"\n"
        ++ "cGHC_SPLIT_PGM        :: String\n"
        ++ "cGHC_SPLIT_PGM        = "ghc-split"\n"
        ++ "cLibFFI               :: Bool\n"
        ++ "cLibFFI               = False\n"
        ++ "cGhcThreaded :: Bool\n"
        ++ "cGhcThreaded = True\n"
        ++ "cGhcDebugged :: Bool\n"
        ++ "cGhcDebugged = False\n"


