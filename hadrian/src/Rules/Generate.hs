module Rules.Generate (
    isGeneratedCmmFile, compilerDependencies, generatePackageCode,
    generateRules, copyRules, generatedDependencies,
    ghcPrimDependencies
    ) where

import Data.Foldable (for_)

import Base
import Expression
import Oracles.ModuleFiles
import Oracles.Setting
import Packages
import Rules.Gmp
import Rules.Libffi
import Settings
import Settings.Builders.DeriveConstants (deriveConstantsPairs)
import Target
import Utilities

-- | Track this file to rebuild generated files whenever it changes.
trackGenerateHs :: Expr ()
trackGenerateHs = expr $ need [sourcePath -/- "Rules/Generate.hs"]

primopsSource :: FilePath
primopsSource = "compiler/prelude/primops.txt.pp"

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
    gmpPath <- expr gmpBuildPath
    rtsPath <- expr (rtsBuildPath stage)
    libDir <- expr $ stageLibPath stage
    mconcat [ return $ (libDir -/-) <$> derivedConstantsFiles
            , notStage0 ? isGmp ? return [gmpPath -/- gmpLibraryH]
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
                  , "primop-vector-uniques.hs-incl" ] ]

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
        generated f = (root -/- dir -/- "**/*.hs") ?== f && not ("**/autogen/*" ?== f)
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
            root -/- "**" -/- dir -/- "Config.hs" %> go generateConfigHs
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
        , "module Config"
        , "  ( module GHC.Version"
        , "  , cBuildPlatformString"
        , "  , cHostPlatformString"
        , "  , cProjectName"
        , "  , cBooterVersion"
        , "  , cStage"
        , "  ) where"
        , ""
        , "import GhcPrelude"
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
