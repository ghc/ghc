module Rules.Generate (generatePackageCode) where

import Expression
import GHC
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
        buildPath -/- "*.hs" %> \file -> do
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

        -- TODO: why different folders for generated files?
        fmap (buildPath -/-)
            [ "GHC/PrimopWrappers.hs"
            , "autogen/GHC/Prim.hs"
            , "*.hs-incl" ] |%> \file -> do
                need [primops]
                build $ fullTarget target GenPrimopCode [primops] [file]

        priority 2.0 $ buildPath -/- "Config.hs" %> \file -> do
            config <- interpretPartial target generateConfig
            writeFileChanged file config
            putBuild $ "| Successfully generated '" ++ file ++ "'."

-- TODO: do we need ghc-split? Always or is it platform specific?
generateConfig :: Expr String
generateConfig = do
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
    return $ "{-# LANGUAGE CPP #-}\n"
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
        ++ "cProjectName          = \"" ++ cProjectName ++ "\"\n"
        ++ "cProjectGitCommitId   :: String\n"
        ++ "cProjectGitCommitId   = \"" ++ cProjectGitCommitId ++ "\"\n"
        ++ "cProjectVersion       :: String\n"
        ++ "cProjectVersion       = \"" ++ cProjectVersion ++ "\"\n"
        ++ "cProjectVersionInt    :: String\n"
        ++ "cProjectVersionInt    = \"" ++ cProjectVersionInt ++ "\"\n"
        ++ "cProjectPatchLevel    :: String\n"
        ++ "cProjectPatchLevel    = \"" ++ cProjectPatchLevel ++ "\"\n"
        ++ "cProjectPatchLevel1   :: String\n"
        ++ "cProjectPatchLevel1   = \"" ++ cProjectPatchLevel1 ++ "\"\n"
        ++ "cProjectPatchLevel2   :: String\n"
        ++ "cProjectPatchLevel2   = \"" ++ cProjectPatchLevel2 ++ "\"\n"
        ++ "cBooterVersion        :: String\n"
        ++ "cBooterVersion        = \"" ++ cBooterVersion ++ "\"\n"
        ++ "cStage                :: String\n"
        ++ "cStage                = show (STAGE :: Int)\n"
        ++ "cIntegerLibrary       :: String\n"
        ++ "cIntegerLibrary       = \"" ++ pkgName integerLibrary ++ "\"\n"
        ++ "cIntegerLibraryType   :: IntegerLibrary\n"
        ++ "cIntegerLibraryType   = " ++ cIntegerLibraryType ++ "\n"
        ++ "cSupportsSplitObjs    :: String\n"
        ++ "cSupportsSplitObjs    = \"" ++ cSupportsSplitObjs ++ "\"\n"
        ++ "cGhcWithInterpreter   :: String\n"
        ++ "cGhcWithInterpreter   = \"" ++ cGhcWithInterpreter ++ "\"\n"
        ++ "cGhcWithNativeCodeGen :: String\n"
        ++ "cGhcWithNativeCodeGen = \"" ++ cGhcWithNativeCodeGen ++ "\"\n"
        ++ "cGhcWithSMP           :: String\n"
        ++ "cGhcWithSMP           = \"" ++ cGhcWithSMP ++ "\"\n"
        ++ "cGhcRTSWays           :: String\n"
        ++ "cGhcRTSWays           = \"" ++ cGhcRTSWays ++ "\"\n"
        ++ "cGhcEnableTablesNextToCode :: String\n"
        ++ "cGhcEnableTablesNextToCode = \"" ++ cGhcEnableTablesNextToCode ++ "\"\n"
        ++ "cLeadingUnderscore    :: String\n"
        ++ "cLeadingUnderscore    = \"" ++ cLeadingUnderscore ++ "\"\n"
        ++ "cGHC_UNLIT_PGM        :: String\n"
        ++ "cGHC_UNLIT_PGM        = \"" ++ cGHC_UNLIT_PGM ++ "\"\n"
        ++ "cGHC_SPLIT_PGM        :: String\n"
        ++ "cGHC_SPLIT_PGM        = \"" ++ cGHC_SPLIT_PGM ++ "\"\n"
        ++ "cLibFFI               :: Bool\n"
        ++ "cLibFFI               = " ++ show cLibFFI ++ "\n"
        ++ "cGhcThreaded :: Bool\n"
        ++ "cGhcThreaded = " ++ show (threaded `elem` rtsWays) ++ "\n"
        ++ "cGhcDebugged :: Bool\n"
        ++ "cGhcDebugged = " ++ show ghcDebugged ++ "\n"
