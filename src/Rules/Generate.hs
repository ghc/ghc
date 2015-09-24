module Rules.Generate (generatePackageCode) where

import Expression
import GHC
import Oracles
import Rules.Actions
import Rules.Resources
import Settings

primopsSource :: FilePath
primopsSource = "compiler/prelude/primops.txt.pp"

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
generatePackageCode _ target @ (PartialTarget stage pkg) =
    let path        = targetPath stage pkg
        packagePath = pkgPath pkg
        buildPath   = path -/- "build"
        primopsTxt  = targetPath stage compiler -/- "build/primops.txt"
        platformH   = targetPath stage compiler -/- "ghc_boot_platform.h"
    in do -- TODO: do we need to copy *.(l)hs-boot files here? Never happens?
        buildPath -/- "*.hs" %> \file -> do
            dirs  <- interpretPartial target $ getPkgDataList SrcDirs
            files <- getDirectoryFiles "" $
                [ packagePath -/- d -/- takeBaseName file <.> "*" | d <- dirs ]
            let gens = [ (f, b) | f <- files, Just b <- [determineBuilder f] ]
            when (length gens /= 1) . putError $
                "Exactly one generator expected for " ++ file
                ++ " (found: " ++ show gens ++ ")."
            let (src, builder) = head gens
            need [src]
            build $ fullTarget target builder [src] [file]

        when (pkg == compiler) $ primopsTxt %> \file -> do
            need [platformH, primopsSource]
            build $ fullTarget target HsCpp [primopsSource] [file]

        -- TODO: why different folders for generated files?
        -- TODO: needing platformH is ugly and fragile
        fmap (buildPath -/-)
            [ "GHC/PrimopWrappers.hs"
            , "autogen/GHC/Prim.hs"
            , "*.hs-incl" ] |%> \file -> do
                need [primopsTxt]
                build $ fullTarget target GenPrimopCode [primopsTxt] [file]

        priority 2.0 $ buildPath -/- "Config.hs" %> \file -> do
            contents <- interpretPartial target generateConfigHs
            writeFileChanged file contents
            putBuild $ "| Successfully generated '" ++ file ++ "'."

        when (pkg == compiler) $ platformH %> \file -> do
            contents <- interpretPartial target generatePlatformH
            writeFileChanged file contents
            putBuild $ "| Successfully generated '" ++ file ++ "'."

quote :: String -> String
quote s = "\"" ++ s ++ "\""

-- TODO: do we need ghc-split? Always or is it platform specific?
-- TODO: add tracking
generateConfigHs :: Expr String
generateConfigHs = do
    cProjectName        <- getSetting ProjectName
    cProjectGitCommitId <- getSetting ProjectGitCommitId
    cProjectVersion     <- getSetting ProjectVersion
    cProjectVersionInt  <- getSetting ProjectVersionInt
    cProjectPatchLevel  <- getSetting ProjectPatchLevel
    cProjectPatchLevel1 <- getSetting ProjectPatchLevel1
    cProjectPatchLevel2 <- getSetting ProjectPatchLevel2
    cBooterVersion      <- getSetting GhcVersion
    let cIntegerLibraryType | integerLibrary == integerGmp    = "IntegerGMP"
                            | integerLibrary == integerSimple = "IntegerSimple"
                            | otherwise = error $ "Unknown integer library: "
                                          ++ show integerLibrary ++ "."
        yesNo = lift . fmap (\x -> if x then "YES" else "NO")
    cSupportsSplitObjs         <- yesNo supportsSplitObjects
    cGhcWithInterpreter        <- yesNo ghcWithInterpreter
    cGhcWithNativeCodeGen      <- yesNo ghcWithNativeCodeGen
    cGhcWithSMP                <- yesNo ghcWithSMP
    cGhcEnableTablesNextToCode <- yesNo ghcEnableTablesNextToCode
    cLeadingUnderscore         <- yesNo $ flag LeadingUnderscore
    cGHC_UNLIT_PGM             <- fmap takeFileName $ getBuilderPath Unlit
    cGHC_SPLIT_PGM             <- fmap takeBaseName $ getBuilderPath GhcSplit
    cLibFFI                    <- lift useLibFFIForAdjustors
    rtsWays                    <- getRtsWays
    let cGhcRTSWays = unwords $ map show rtsWays
    return $ unlines
        [ "{-# LANGUAGE CPP #-}"
        , "module Config where"
        , ""
        , "#include \"ghc_boot_platform.h\""
        , ""
        , "data IntegerLibrary = IntegerGMP"
        , "                    | IntegerSimple"
        , "                    deriving Eq"
        , ""
        , "cBuildPlatformString :: String"
        , "cBuildPlatformString = BuildPlatform_NAME"
        , "cHostPlatformString :: String"
        , "cHostPlatformString = HostPlatform_NAME"
        , "cTargetPlatformString :: String"
        , "cTargetPlatformString = TargetPlatform_NAME"
        , ""
        , "cProjectName          :: String"
        , "cProjectName          = " ++ quote cProjectName
        , "cProjectGitCommitId   :: String"
        , "cProjectGitCommitId   = " ++ quote cProjectGitCommitId
        , "cProjectVersion       :: String"
        , "cProjectVersion       = " ++ quote cProjectVersion
        , "cProjectVersionInt    :: String"
        , "cProjectVersionInt    = " ++ quote cProjectVersionInt
        , "cProjectPatchLevel    :: String"
        , "cProjectPatchLevel    = " ++ quote cProjectPatchLevel
        , "cProjectPatchLevel1   :: String"
        , "cProjectPatchLevel1   = " ++ quote cProjectPatchLevel1
        , "cProjectPatchLevel2   :: String"
        , "cProjectPatchLevel2   = " ++ quote cProjectPatchLevel2
        , "cBooterVersion        :: String"
        , "cBooterVersion        = " ++ quote cBooterVersion
        , "cStage                :: String"
        , "cStage                = show (STAGE :: Int)"
        , "cIntegerLibrary       :: String"
        , "cIntegerLibrary       = " ++ quote (pkgName integerLibrary)
        , "cIntegerLibraryType   :: IntegerLibrary"
        , "cIntegerLibraryType   = " ++ cIntegerLibraryType
        , "cSupportsSplitObjs    :: String"
        , "cSupportsSplitObjs    = " ++ quote cSupportsSplitObjs
        , "cGhcWithInterpreter   :: String"
        , "cGhcWithInterpreter   = " ++ quote cGhcWithInterpreter
        , "cGhcWithNativeCodeGen :: String"
        , "cGhcWithNativeCodeGen = " ++ quote cGhcWithNativeCodeGen
        , "cGhcWithSMP           :: String"
        , "cGhcWithSMP           = " ++ quote cGhcWithSMP
        , "cGhcRTSWays           :: String"
        , "cGhcRTSWays           = " ++ quote cGhcRTSWays
        , "cGhcEnableTablesNextToCode :: String"
        , "cGhcEnableTablesNextToCode = " ++ quote cGhcEnableTablesNextToCode
        , "cLeadingUnderscore    :: String"
        , "cLeadingUnderscore    = " ++ quote cLeadingUnderscore
        , "cGHC_UNLIT_PGM        :: String"
        , "cGHC_UNLIT_PGM        = " ++ quote cGHC_UNLIT_PGM
        , "cGHC_SPLIT_PGM        :: String"
        , "cGHC_SPLIT_PGM        = " ++ quote cGHC_SPLIT_PGM
        , "cLibFFI               :: Bool"
        , "cLibFFI               = " ++ show cLibFFI
        , "cGhcThreaded :: Bool"
        , "cGhcThreaded = " ++ show (threaded `elem` rtsWays)
        , "cGhcDebugged :: Bool"
        , "cGhcDebugged = " ++ show ghcDebugged ]

generatePlatformH :: Expr String
generatePlatformH = do
    stage <- getStage
    let cppify = replaceEq '-' '_' . replaceEq '.' '_'
        chooseSetting x y = getSetting $ if stage == Stage0 then x else y
    buildPlatform  <- chooseSetting BuildPlatform HostPlatform
    buildArch      <- chooseSetting BuildArch     HostArch
    buildOs        <- chooseSetting BuildOs       HostOs
    buildVendor    <- chooseSetting BuildVendor   HostVendor
    hostPlatform   <- chooseSetting HostPlatform  TargetPlatform
    hostArch       <- chooseSetting HostArch      TargetArch
    hostOs         <- chooseSetting HostOs        TargetOs
    hostVendor     <- chooseSetting HostVendor    TargetVendor
    targetPlatform <- getSetting TargetPlatform
    targetArch     <- getSetting TargetArch
    targetOs       <- getSetting TargetOs
    targetVendor   <- getSetting TargetVendor
    return $ unlines
        [ "#ifndef __PLATFORM_H__"
        , "#define __PLATFORM_H__"
        , ""
        , "#define BuildPlatform_NAME  " ++ quote buildPlatform
        , "#define HostPlatform_NAME   " ++ quote hostPlatform
        , "#define TargetPlatform_NAME " ++ quote targetPlatform
        , ""
        , "#define " ++ cppify buildPlatform  ++ "_BUILD 1"
        , "#define " ++ cppify hostPlatform   ++ "_HOST 1"
        , "#define " ++ cppify targetPlatform ++ "_TARGET 1"
        , ""
        , "#define " ++ buildArch  ++ "_BUILD_ARCH 1"
        , "#define " ++ hostArch   ++ "_HOST_ARCH 1"
        , "#define " ++ targetArch ++ "_TARGET_ARCH 1"
        , "#define BUILD_ARCH "  ++ quote buildArch
        , "#define HOST_ARCH "   ++ quote hostArch
        , "#define TARGET_ARCH " ++ quote targetArch
        , ""
        , "#define " ++ buildOs  ++ "_BUILD_OS 1"
        , "#define " ++ hostOs   ++ "_HOST_OS 1"
        , "#define " ++ targetOs ++ "_TARGET_OS 1"
        , "#define BUILD_OS "  ++ quote buildOs
        , "#define HOST_OS "   ++ quote hostOs
        , "#define TARGET_OS " ++ quote targetOs
        , ""
        , "#define " ++ buildVendor  ++ "_BUILD_VENDOR 1"
        , "#define " ++ hostVendor   ++ "_HOST_VENDOR 1"
        , "#define " ++ targetVendor ++ "_TARGET_VENDOR  1"
        , "#define BUILD_VENDOR "  ++ quote buildVendor
        , "#define HOST_VENDOR "   ++ quote hostVendor
        , "#define TARGET_VENDOR " ++ quote targetVendor
        , ""
        , "#endif /* __PLATFORM_H__ */" ]
