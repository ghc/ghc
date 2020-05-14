module Rules.Generate (
    isGeneratedCmmFile, compilerDependencies, generatePackageCode,
    generateRules, copyRules, generatedDependencies,
    ghcPrimDependencies
    ) where

import Data.Foldable (for_)

import Base
import qualified Context
import Expression
import Flavour
import Hadrian.Oracles.TextFile (lookupValueOrError)
import Oracles.Flag
import Oracles.ModuleFiles
import Oracles.Setting
import Packages
import Rules.Libffi
import Settings
import Settings.Builders.DeriveConstants (deriveConstantsPairs)
import Target
import Utilities

-- | Track this file to rebuild generated files whenever it changes.
trackGenerateHs :: Expr ()
trackGenerateHs = expr $ need [sourcePath -/- "Rules/Generate.hs"]

primopsSource :: FilePath
primopsSource = "compiler/GHC/Builtin/primops.txt.pp"

primopsTxt :: Stage -> FilePath
primopsTxt stage = buildDir (vanillaContext stage compiler) -/- "primops.txt"

isGeneratedCmmFile :: FilePath -> Bool
isGeneratedCmmFile file = takeBaseName file == "AutoApply"

ghcPrimDependencies :: Expr [FilePath]
ghcPrimDependencies = do
    stage <- getStage
    path  <- expr $ buildPath (vanillaContext stage ghcPrim)
    return [path -/- "GHC/Prim.hs", path -/- "GHC/PrimopWrappers.hs"]

derivedConstantsFiles :: [FilePath]
derivedConstantsFiles =
    [ "DerivedConstants.h"
    , "GHCConstantsHaskellExports.hs"
    , "GHCConstantsHaskellType.hs"
    , "GHCConstantsHaskellWrappers.hs" ]

compilerDependencies :: Expr [FilePath]
compilerDependencies = do
    stage   <- getStage
    isGmp   <- (== integerGmp) <$> getIntegerPackage
    ghcPath <- expr $ buildPath (vanillaContext stage compiler)
    gmpPath <- expr $ buildPath (vanillaContext stage integerGmp)
    rtsPath <- expr (rtsBuildPath stage)
    libDir <- expr $ stageLibPath stage
    mconcat [ return $ (libDir -/-) <$> derivedConstantsFiles
            , notStage0 ? isGmp ? return [gmpPath -/- "include/ghc-gmp.h"]
            , notStage0 ? return ((rtsPath -/-) <$> libffiHeaderFiles)
            , return $ fmap (ghcPath -/-)
                  [ "primop-can-fail.hs-incl"
                  , "primop-code-size.hs-incl"
                  , "primop-commutable.hs-incl"
                  , "primop-data-decl.hs-incl"
                  , "primop-fixity.hs-incl"
                  , "primop-has-side-effects.hs-incl"
                  , "primop-list.hs-incl"
                  , "primop-out-of-line.hs-incl"
                  , "primop-primop-info.hs-incl"
                  , "primop-strictness.hs-incl"
                  , "primop-tag.hs-incl"
                  , "primop-vector-tycons.hs-incl"
                  , "primop-vector-tys-exports.hs-incl"
                  , "primop-vector-tys.hs-incl"
                  , "primop-vector-uniques.hs-incl"
                  , "primop-docs.hs-incl" ] ]

generatedDependencies :: Expr [FilePath]
generatedDependencies = do
    stage    <- getStage
    rtsPath  <- expr (rtsBuildPath stage)
    includes <- expr $ includesDependencies stage
    libDir <- expr $ stageLibPath stage
    mconcat [ package compiler ? compilerDependencies
            , package ghcPrim  ? ghcPrimDependencies
            , package rts      ? return (fmap (rtsPath -/-) libffiHeaderFiles
                ++ includes
                ++ ((libDir -/-) <$> derivedConstantsFiles))
            , stage0 ? return includes ]

generate :: FilePath -> Context -> Expr String -> Action ()
generate file context expr = do
    contents <- interpretInContext context expr
    writeFileChanged file contents
    putSuccess $ "| Successfully generated " ++ file ++ "."

generatePackageCode :: Context -> Rules ()
generatePackageCode context@(Context stage pkg _) = do
    root <- buildRootRules
    let dir         = buildDir context
        generated f = (root -/- dir -/- "**/*.hs") ?== f && not ("//autogen/*" ?== f)
        go gen file = generate file context gen
    generated ?> \file -> do
        let unpack = fromMaybe . error $ "No generator for " ++ file ++ "."
        (src, builder) <- unpack <$> findGenerator context file
        need [src]
        build $ target context builder [src] [file]
        let boot = src -<.> "hs-boot"
        whenM (doesFileExist boot) $ do
            let target = file -<.> "hs-boot"
            copyFile boot target
            produces [target]

    priority 2.0 $ do
        when (pkg == compiler) $ do
            root -/- "**" -/- dir -/- "GHC/Settings/Config.hs" %> go generateConfigHs
            root -/- "**" -/- dir -/- "*.hs-incl" %> genPrimopCode context
        when (pkg == ghcPrim) $ do
            root -/- "**" -/- dir -/- "GHC/Prim.hs" %> genPrimopCode context
            root -/- "**" -/- dir -/- "GHC/PrimopWrappers.hs" %> genPrimopCode context
        when (pkg == ghcBoot) $ do
            root -/- "**" -/- dir -/- "GHC/Version.hs" %> go generateVersionHs
            root -/- "**" -/- dir -/- "GHC/Platform/Host.hs" %> go generatePlatformHostHs

    when (pkg == compiler) $ do
        root -/- primopsTxt stage %> \file -> do
            includes <- includesDependencies stage
            need $ [primopsSource] ++ includes
            build $ target context HsCpp [primopsSource] [file]

    when (pkg == rts) $ do
        root -/- "**" -/- dir -/- "cmm/AutoApply.cmm" %> \file ->
            build $ target context GenApply [] [file]
        -- TODO: This should be fixed properly, e.g. generated here on demand.
        (root -/- "**" -/- dir -/- "DerivedConstants.h") <~ stageLibPath stage
        (root -/- "**" -/- dir -/- "ghcautoconf.h") <~ stageLibPath stage
        (root -/- "**" -/- dir -/- "ghcplatform.h") <~ stageLibPath stage
        (root -/- "**" -/- dir -/- "ghcversion.h") <~ stageLibPath stage
 where
    pattern <~ mdir = pattern %> \file -> do
        dir <- mdir
        copyFile (dir -/- takeFileName file) file

genPrimopCode :: Context -> FilePath -> Action ()
genPrimopCode context@(Context stage _pkg _) file = do
    root <- buildRoot
    need [root -/- primopsTxt stage]
    build $ target context GenPrimopCode [root -/- primopsTxt stage] [file]

copyRules :: Rules ()
copyRules = do
    root <- buildRootRules
    forM_ [Stage0 ..] $ \stage -> do
        let prefix = root -/- stageString stage -/- "lib"

            infixl 1 <~
            pattern <~ mdir = pattern %> \file -> do
                dir <- mdir
                copyFile (dir -/- makeRelative prefix file) file

        prefix -/- "ghc-usage.txt"     <~ return "driver"
        prefix -/- "ghci-usage.txt"    <~ return "driver"
        prefix -/- "llvm-targets"      <~ return "."
        prefix -/- "llvm-passes"       <~ return "."
        prefix -/- "template-hsc.h"    <~ return (pkgPath hsc2hs)

        prefix -/- "html/**"           <~ return "utils/haddock/haddock-api/resources"
        prefix -/- "latex/**"          <~ return "utils/haddock/haddock-api/resources"

generateRules :: Rules ()
generateRules = do
    root <- buildRootRules

    (root -/- "ghc-stage1") <~+ ghcWrapper Stage1
    (root -/- "ghc-stage2") <~+ ghcWrapper Stage2

    forM_ [Stage0 ..] $ \stage -> do
        let prefix = root -/- stageString stage -/- "lib"
            go gen file = generate file (semiEmptyTarget stage) gen
        (prefix -/- "ghcplatform.h") %> go generateGhcPlatformH
        (prefix -/- "settings") %> go generateSettings
        (prefix -/- "ghcautoconf.h") %> go generateGhcAutoconfH
        (prefix -/- "ghcversion.h") %> go generateGhcVersionH
        -- TODO: simplify, get rid of fake rts context
        for_ (fst <$> deriveConstantsPairs) $ \constantsFile ->
            prefix -/- constantsFile %> \file -> do
                withTempDir $ \dir -> build $
                    target (rtsContext stage) DeriveConstants [] [file, dir]
  where
    file <~+ gen = file %> \out -> generate out emptyTarget gen >> makeExecutable out

-- TODO: Use the Types, Luke! (drop partial function)
-- We sometimes need to evaluate expressions that do not require knowing all
-- information about the context. In this case, we don't want to know anything.
semiEmptyTarget :: Stage -> Context
semiEmptyTarget stage = vanillaContext stage
  (error "Rules.Generate.emptyTarget: unknown package")

emptyTarget :: Context
emptyTarget = vanillaContext (error "Rules.Generate.emptyTarget: unknown stage")
                             (error "Rules.Generate.emptyTarget: unknown package")

-- Generators

-- | GHC wrapper scripts used for passing the path to the right package database
-- when invoking in-tree GHC executables.
ghcWrapper :: Stage -> Expr String
ghcWrapper Stage0 = error "Stage0 GHC does not require a wrapper script to run."
ghcWrapper stage  = do
    dbPath  <- expr $ (</>) <$> topDirectory <*> packageDbPath stage
    ghcPath <- expr $ (</>) <$> topDirectory
                            <*> programPath (vanillaContext (pred stage) ghc)
    return $ unwords $ map show $ [ ghcPath ]
                               ++ [ "-package-db " ++ dbPath | stage == Stage1 ]
                               ++ [ "$@" ]

-- | Given a 'String' replace characters '.' and '-' by underscores ('_') so that
-- the resulting 'String' is a valid C preprocessor identifier.
cppify :: String -> String
cppify = replaceEq '-' '_' . replaceEq '.' '_'

-- | Generate @ghcplatform.h@ header.
generateGhcPlatformH :: Expr String
generateGhcPlatformH = do
    trackGenerateHs
    stage    <- getStage
    let chooseSetting x y = getSetting $ if stage == Stage0 then x else y
    buildPlatform  <- chooseSetting BuildPlatform HostPlatform
    buildArch      <- chooseSetting BuildArch     HostArch
    buildOs        <- chooseSetting BuildOs       HostOs
    buildVendor    <- chooseSetting BuildVendor   HostVendor
    hostPlatform   <- chooseSetting HostPlatform  TargetPlatform
    hostArch       <- chooseSetting HostArch      TargetArch
    hostOs         <- chooseSetting HostOs        TargetOs
    hostVendor     <- chooseSetting HostVendor    TargetVendor
    ghcUnreg       <- getFlag    GhcUnregisterised
    return . unlines $
        [ "#if !defined(__GHCPLATFORM_H__)"
        , "#define __GHCPLATFORM_H__"
        , ""
        , "#define GHC_STAGE " ++ show (fromEnum stage + 1)
        , ""
        , "#define BuildPlatform_TYPE  " ++ cppify buildPlatform
        , "#define HostPlatform_TYPE   " ++ cppify hostPlatform
        , ""
        , "#define " ++ cppify buildPlatform   ++ "_BUILD 1"
        , "#define " ++ cppify hostPlatform ++ "_HOST 1"
        , ""
        , "#define " ++ buildArch   ++ "_BUILD_ARCH 1"
        , "#define " ++ hostArch ++ "_HOST_ARCH 1"
        , "#define BUILD_ARCH " ++ show buildArch
        , "#define HOST_ARCH "  ++ show hostArch
        , ""
        , "#define " ++ buildOs   ++ "_BUILD_OS 1"
        , "#define " ++ hostOs ++ "_HOST_OS 1"
        , "#define BUILD_OS " ++ show buildOs
        , "#define HOST_OS "  ++ show hostOs
        , ""
        , "#define " ++ buildVendor   ++ "_BUILD_VENDOR 1"
        , "#define " ++ hostVendor ++ "_HOST_VENDOR 1"
        , "#define BUILD_VENDOR " ++ show buildVendor
        , "#define HOST_VENDOR "  ++ show hostVendor
        , ""
        ]
        ++
        [ "#define UnregisterisedCompiler 1" | ghcUnreg ]
        ++
        [ ""
        , "#endif /* __GHCPLATFORM_H__ */"
        ]

generateSettings :: Expr String
generateSettings = do
    ctx <- getContext
    settings <- traverse sequence $
        [ ("GCC extra via C opts", expr $ lookupValueOrError configFile "gcc-extra-via-c-opts")
        , ("C compiler command", expr $ settingsFileSetting SettingsFileSetting_CCompilerCommand)
        , ("C compiler flags", expr $ settingsFileSetting SettingsFileSetting_CCompilerFlags)
        , ("C++ compiler flags", expr $ settingsFileSetting SettingsFileSetting_CxxCompilerFlags)
        , ("C compiler link flags", expr $ settingsFileSetting SettingsFileSetting_CCompilerLinkFlags)
        , ("C compiler supports -no-pie", expr $ settingsFileSetting SettingsFileSetting_CCompilerSupportsNoPie)
        , ("Haskell CPP command", expr $ settingsFileSetting SettingsFileSetting_HaskellCPPCommand)
        , ("Haskell CPP flags", expr $ settingsFileSetting SettingsFileSetting_HaskellCPPFlags)
        , ("ld command", expr $ settingsFileSetting SettingsFileSetting_LdCommand)
        , ("ld flags", expr $ settingsFileSetting SettingsFileSetting_LdFlags)
        , ("ld supports compact unwind", expr $ lookupValueOrError configFile "ld-has-no-compact-unwind")
        , ("ld supports build-id", expr $ lookupValueOrError configFile "ld-has-build-id")
        , ("ld supports filelist", expr $ lookupValueOrError configFile "ld-has-filelist")
        , ("ld is GNU ld", expr $ lookupValueOrError configFile "ld-is-gnu-ld")
        , ("ar command", expr $ settingsFileSetting SettingsFileSetting_ArCommand)
        , ("ar flags", expr $ lookupValueOrError configFile "ar-args")
        , ("ar supports at file", expr $ yesNo <$> flag ArSupportsAtFile)
        , ("ranlib command", expr $ settingsFileSetting SettingsFileSetting_RanlibCommand)
        , ("touch command", expr $ settingsFileSetting SettingsFileSetting_TouchCommand)
        , ("dllwrap command", expr $ settingsFileSetting SettingsFileSetting_DllWrapCommand)
        , ("windres command", expr $ settingsFileSetting SettingsFileSetting_WindresCommand)
        , ("libtool command", expr $ settingsFileSetting SettingsFileSetting_LibtoolCommand)
        , ("unlit command", ("$topdir/bin/" <>) <$> expr (programName (ctx { Context.package = unlit })))
        , ("cross compiling", expr $ yesNo <$> flag CrossCompiling)
        , ("target platform string", getSetting TargetPlatform)
        , ("target os", getSetting TargetOsHaskell)
        , ("target arch", getSetting TargetArchHaskell)
        , ("target word size", expr $ lookupValueOrError configFile "target-word-size")
        , ("target word big endian", expr $ lookupValueOrError configFile "target-word-big-endian")
        , ("target has GNU nonexec stack", expr $ lookupValueOrError configFile "target-has-gnu-nonexec-stack")
        , ("target has .ident directive", expr $ lookupValueOrError configFile "target-has-ident-directive")
        , ("target has subsections via symbols", expr $ lookupValueOrError configFile "target-has-subsections-via-symbols")
        , ("target has RTS linker", expr $ lookupValueOrError configFile "target-has-rts-linker")
        , ("Unregisterised", expr $ yesNo <$> flag GhcUnregisterised)
        , ("LLVM target", getSetting LlvmTarget)
        , ("LLVM llc command", expr $ settingsFileSetting SettingsFileSetting_LlcCommand)
        , ("LLVM opt command", expr $ settingsFileSetting SettingsFileSetting_OptCommand)
        , ("LLVM clang command", expr $ settingsFileSetting SettingsFileSetting_ClangCommand)

        , ("integer library", pkgName <$> getIntegerPackage)
        , ("Use interpreter", expr $ yesNo <$> ghcWithInterpreter)
        , ("Use native code generator", expr $ yesNo <$> ghcWithNativeCodeGen)
        , ("Support SMP", expr $ yesNo <$> targetSupportsSMP)
        , ("RTS ways", unwords . map show <$> getRtsWays)
        , ("Tables next to code", expr $ yesNo <$> flag TablesNextToCode)
        , ("Leading underscore", expr $ yesNo <$> flag LeadingUnderscore)
        , ("Use LibFFI", expr $ yesNo <$> useLibFFIForAdjustors)
        , ("Use Threads", expr $ yesNo . ghcThreaded <$> flavour)
        , ("Use Debugging", expr $ yesNo . ghcDebugged <$> flavour)
        , ("RTS expects libdw", yesNo <$> getFlag WithLibdw)
        ]
    let showTuple (k, v) = "(" ++ show k ++ ", " ++ show v ++ ")"
    pure $ case settings of
        [] -> "[]"
        s : ss -> unlines $
            ("[" ++ showTuple s)
            : ((\s' -> "," ++ showTuple s') <$> ss)
            ++ ["]"]


-- | Generate @Config.hs@ files.
generateConfigHs :: Expr String
generateConfigHs = do
    stage <- getStage
    let chooseSetting x y = getSetting $ if stage == Stage0 then x else y
    buildPlatform <- chooseSetting BuildPlatform HostPlatform
    hostPlatform <- chooseSetting HostPlatform TargetPlatform
    trackGenerateHs
    cProjectName        <- getSetting ProjectName
    cBooterVersion      <- getSetting GhcVersion
    return $ unlines
        [ "{-# LANGUAGE CPP #-}"
        , "module GHC.Settings.Config"
        , "  ( module GHC.Version"
        , "  , cBuildPlatformString"
        , "  , cHostPlatformString"
        , "  , cProjectName"
        , "  , cBooterVersion"
        , "  , cStage"
        , "  ) where"
        , ""
        , "import GHC.Prelude"
        , ""
        , "import GHC.Version"
        , ""
        , "cBuildPlatformString :: String"
        , "cBuildPlatformString = " ++ show buildPlatform
        , ""
        , "cHostPlatformString :: String"
        , "cHostPlatformString = " ++ show hostPlatform
        , ""
        , "cProjectName          :: String"
        , "cProjectName          = " ++ show cProjectName
        , ""
        , "cBooterVersion        :: String"
        , "cBooterVersion        = " ++ show cBooterVersion
        , ""
        , "cStage                :: String"
        , "cStage                = show (" ++ show (fromEnum stage + 1) ++ " :: Int)"
        ]

-- | Generate @ghcautoconf.h@ header.
generateGhcAutoconfH :: Expr String
generateGhcAutoconfH = do
    trackGenerateHs
    configHContents  <- expr $ map undefinePackage <$> readFileLines configH
    return . unlines $
        [ "#if !defined(__GHCAUTOCONF_H__)"
        , "#define __GHCAUTOCONF_H__" ]
        ++ configHContents ++
        [ "#endif /* __GHCAUTOCONF_H__ */" ]
  where
    undefinePackage s
        | "#define PACKAGE_" `isPrefixOf` s
            = "/* #undef " ++ takeWhile (/=' ') (drop 8 s) ++ " */"
        | otherwise = s

-- | Generate @ghcversion.h@ header.
generateGhcVersionH :: Expr String
generateGhcVersionH = do
    trackGenerateHs
    version     <- getSetting ProjectVersionInt
    patchLevel1 <- getSetting ProjectPatchLevel1
    patchLevel2 <- getSetting ProjectPatchLevel2
    return . unlines $
        [ "#if !defined(__GHCVERSION_H__)"
        , "#define __GHCVERSION_H__"
        , ""
        , "#if !defined(__GLASGOW_HASKELL__)"
        , "# define __GLASGOW_HASKELL__ " ++ version
        , "#endif"
        , ""]
        ++
        [ "#define __GLASGOW_HASKELL_PATCHLEVEL1__ " ++ patchLevel1 | patchLevel1 /= "" ]
        ++
        [ "#define __GLASGOW_HASKELL_PATCHLEVEL2__ " ++ patchLevel2 | patchLevel2 /= "" ]
        ++
        [ ""
        , "#define MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2) (\\"
        , "   ((ma)*100+(mi)) <  __GLASGOW_HASKELL__ || \\"
        , "   ((ma)*100+(mi)) == __GLASGOW_HASKELL__    \\"
        , "          && (pl1) <  __GLASGOW_HASKELL_PATCHLEVEL1__ || \\"
        , "   ((ma)*100+(mi)) == __GLASGOW_HASKELL__    \\"
        , "          && (pl1) == __GLASGOW_HASKELL_PATCHLEVEL1__ \\"
        , "          && (pl2) <= __GLASGOW_HASKELL_PATCHLEVEL2__ )"
        , ""
        , "#endif /* __GHCVERSION_H__ */" ]

-- | Generate @Version.hs@ files.
generateVersionHs :: Expr String
generateVersionHs = do
    trackGenerateHs
    cProjectGitCommitId <- getSetting ProjectGitCommitId
    cProjectVersion     <- getSetting ProjectVersion
    cProjectVersionInt  <- getSetting ProjectVersionInt
    cProjectPatchLevel  <- getSetting ProjectPatchLevel
    cProjectPatchLevel1 <- getSetting ProjectPatchLevel1
    cProjectPatchLevel2 <- getSetting ProjectPatchLevel2
    return $ unlines
        [ "module GHC.Version where"
        , ""
        , "import Prelude -- See Note [Why do we import Prelude here?]"
        , ""
        , "cProjectGitCommitId   :: String"
        , "cProjectGitCommitId   = " ++ show cProjectGitCommitId
        , ""
        , "cProjectVersion       :: String"
        , "cProjectVersion       = " ++ show cProjectVersion
        , ""
        , "cProjectVersionInt    :: String"
        , "cProjectVersionInt    = " ++ show cProjectVersionInt
        , ""
        , "cProjectPatchLevel    :: String"
        , "cProjectPatchLevel    = " ++ show cProjectPatchLevel
        , ""
        , "cProjectPatchLevel1   :: String"
        , "cProjectPatchLevel1   = " ++ show cProjectPatchLevel1
        , ""
        , "cProjectPatchLevel2   :: String"
        , "cProjectPatchLevel2   = " ++ show cProjectPatchLevel2
        ]

-- | Generate @Platform/Host.hs@ files.
generatePlatformHostHs :: Expr String
generatePlatformHostHs = do
    trackGenerateHs
    cHostPlatformArch <- getSetting HostArchHaskell
    cHostPlatformOS   <- getSetting HostOsHaskell
    return $ unlines
        [ "module GHC.Platform.Host where"
        , ""
        , "import GHC.Platform"
        , ""
        , "cHostPlatformArch :: Arch"
        , "cHostPlatformArch = " ++ cHostPlatformArch
        , ""
        , "cHostPlatformOS   :: OS"
        , "cHostPlatformOS   = " ++ cHostPlatformOS
        , ""
        , "cHostPlatformMini :: PlatformMini"
        , "cHostPlatformMini = PlatformMini"
        , "  { platformMini_arch = cHostPlatformArch"
        , "  , platformMini_os = cHostPlatformOS"
        , "  }"
        ]
