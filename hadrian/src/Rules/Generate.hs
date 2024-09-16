module Rules.Generate (
    isGeneratedCmmFile, compilerDependencies, generatePackageCode,
    generateRules, copyRules, generatedDependencies,
    ghcPrimDependencies,
    templateRules
    ) where

import Development.Shake.FilePath
import Data.Char (isSpace)
import qualified Data.Set as Set
import Base
import qualified Context
import Expression
import Hadrian.Oracles.TextFile (lookupSystemConfig)
import Oracles.Flag hiding (arSupportsAtFile, arSupportsDashL)
import Oracles.ModuleFiles
import Oracles.Setting
import Hadrian.Haskell.Cabal.Type (PackageData(version))
import Hadrian.Haskell.Cabal
import Hadrian.Oracles.Cabal (readPackageData)
import Packages
import Rules.Libffi
import Settings
import Target
import Utilities

import GHC.Toolchain as Toolchain hiding (HsCpp(HsCpp))
import GHC.Toolchain.Program
import GHC.Platform.ArchOS

-- | Track this file to rebuild generated files whenever it changes.
trackGenerateHs :: Expr ()
trackGenerateHs = expr $ need [sourcePath -/- "Rules/Generate.hs"]

primopsSource :: FilePath
primopsSource = "compiler/GHC/Builtin/primops.txt.pp"

primopsTxt :: Stage -> FilePath
primopsTxt stage = buildDir (vanillaContext stage compiler) -/- "primops.txt"

isGeneratedCmmFile :: FilePath -> Bool
isGeneratedCmmFile file =
  takeBaseName file `elem`
    [ "AutoApply"
    , "AutoApply_V16"
    , "AutoApply_V32"
    , "AutoApply_V64"
    ]

ghcPrimDependencies :: Expr [FilePath]
ghcPrimDependencies = do
    stage <- getStage
    path  <- expr $ buildPath (vanillaContext stage ghcPrim)
    return [path -/- "GHC/Prim.hs", path -/- "GHC/PrimopWrappers.hs"]

rtsDependencies :: Expr [FilePath]
rtsDependencies = do
    stage   <- getStage
    rtsPath <- expr (rtsBuildPath stage)
    jsTarget <- expr isJsTarget
    useSystemFfi <- expr (flag UseSystemFfi)

    let -- headers common to native and JS RTS
        common_headers =
            [ "ghcautoconf.h", "ghcplatform.h"
            , "DerivedConstants.h"
            ]
        -- headers specific to the native RTS
        native_headers =
            [ "rts" -/- "EventTypes.h"
            , "rts" -/- "EventLogConstants.h"
            ]
            ++ (if useSystemFfi then [] else libffiHeaderFiles)
        headers
          | jsTarget  = common_headers
          | otherwise = common_headers ++ native_headers
    pure $ ((rtsPath -/- "include") -/-) <$> headers

compilerDependencies :: Expr [FilePath]
compilerDependencies = do
    let fixed = ("compiler" -/-) <$>
                  [ "GHC/CmmToLlvm/Version/Bounds.hs"
                  ]
    stage   <- getStage
    ghcPath <- expr $ buildPath (vanillaContext stage compiler)
    let buildSpecific = (ghcPath -/-) <$>
                  [ "primop-code-size.hs-incl"
                  , "primop-commutable.hs-incl"
                  , "primop-data-decl.hs-incl"
                  , "primop-fixity.hs-incl"
                  , "primop-effects.hs-incl"
                  , "primop-list.hs-incl"
                  , "primop-out-of-line.hs-incl"
                  , "primop-primop-info.hs-incl"
                  , "primop-strictness.hs-incl"
                  , "primop-is-work-free.hs-incl"
                  , "primop-is-cheap.hs-incl"
                  , "primop-tag.hs-incl"
                  , "primop-vector-tycons.hs-incl"
                  , "primop-vector-tys-exports.hs-incl"
                  , "primop-vector-tys.hs-incl"
                  , "primop-vector-uniques.hs-incl"
                  , "primop-docs.hs-incl"
                  , "primop-deprecations.hs-incl"
                  , "GHC/Platform/Constants.hs"
                  , "GHC/Settings/Config.hs"
                  ]
    pure $ fixed ++ buildSpecific

generatedDependencies :: Expr [FilePath]
generatedDependencies = do
    mconcat [ package compiler ? compilerDependencies
            , package ghcPrim  ? ghcPrimDependencies
            , package rts      ? rtsDependencies
            ]

generate :: FilePath -> Context -> Expr String -> Action ()
generate file context expr = do
    contents <- interpretInContext context expr
    writeFileChanged file contents
    putSuccess $ "| Successfully generated " ++ file ++ "."

generatePackageCode :: Context -> Rules ()
generatePackageCode context@(Context stage pkg _ _) = do
    root <- buildRootRules
    let dir         = buildDir context
        generated f = (root -/- dir -/- "**/*.hs") ?== f && not ("//autogen/*" ?== f)
        go gen file = generate file context gen
    generated ?> \file -> do
        let unpack = fromMaybe . error $ "No generator for " ++ file ++ "."
        (src, builder) <- unpack <$> findGenerator context file
        -- Make sure we have configured the package before running the builder
        pkg_setup <- pkgSetupConfigFile context
        need [src, pkg_setup]
        build $ target context builder [src] [file]
        let boot = src -<.> "hs-boot"
        whenM (doesFileExist boot) $ do
            let target = file -<.> "hs-boot"
            copyFile boot target
            produces [target]

    priority 2.0 $ do
        when (pkg == compiler) $ do
            root -/- "**" -/- dir -/- "GHC/Platform/Constants.hs" %> genPlatformConstantsType context
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
            need $ [primopsSource]
            build $ target context HsCpp [primopsSource] [file]

    when (pkg == rts) $ do
        root -/- "**" -/- dir -/- "cmm/AutoApply.cmm" %> \file -> do
            -- See Note [How genapply gets target info] for details
            path <- buildPath context
            let h = path -/- "include/DerivedConstants.h"
            need [h]
            build $ target context (GenApply Nothing) [h] [file]
        root -/- "**" -/- dir -/- "cmm/AutoApply_V16.cmm" %> \file -> do
            path <- buildPath context
            let h = path -/- "include/DerivedConstants.h"
            need [h]
            build $ target context (GenApply (Just 16)) [h] [file]
        root -/- "**" -/- dir -/- "cmm/AutoApply_V32.cmm" %> \file -> do
            path <- buildPath context
            let h = path -/- "include/DerivedConstants.h"
            need [h]
            build $ target context (GenApply (Just 32)) [h] [file]
        root -/- "**" -/- dir -/- "cmm/AutoApply_V64.cmm" %> \file -> do
            path <- buildPath context
            let h = path -/- "include/DerivedConstants.h"
            need [h]
            build $ target context (GenApply (Just 64)) [h] [file]
        root -/- "**" -/- dir -/- "include/ghcautoconf.h" %> \_ ->
            need . pure =<< pkgSetupConfigFile context
        root -/- "**" -/- dir -/- "include/ghcplatform.h" %> \_ ->
            need . pure =<< pkgSetupConfigFile context
        root -/- "**" -/- dir -/- "include/DerivedConstants.h" %> genPlatformConstantsHeader context
        root -/- "**" -/- dir -/- "include/rts/EventLogConstants.h" %> genEventTypes "--event-types-defines"
        root -/- "**" -/- dir -/- "include/rts/EventTypes.h" %> genEventTypes "--event-types-array"

genEventTypes :: String -> FilePath -> Action ()
genEventTypes flag file = do
    need ["rts" -/- "gen_event_types.py"]
    runBuilder Python
      ["rts" -/- "gen_event_types.py", flag, file]
      [] []

genPrimopCode :: Context -> FilePath -> Action ()
genPrimopCode context@(Context stage _pkg _ _) file = do
    root <- buildRoot
    need [root -/- primopsTxt stage]
    build $ target context GenPrimopCode [root -/- primopsTxt stage] [file]

genPlatformConstantsType :: Context -> FilePath -> Action ()
genPlatformConstantsType context file = do
    withTempDir $ \tmpdir ->
      build $ target context DeriveConstants [] [file,tmpdir]

genPlatformConstantsHeader :: Context -> FilePath -> Action ()
genPlatformConstantsHeader context file = do
    -- N.B. deriveConstants needs to compile programs which #include
    -- PosixSource.h, which #include's ghcplatform.h. Fixes #18290.
    let prefix = takeDirectory file
    need
        [ prefix -/- "ghcplatform.h"
        , prefix -/- "ghcautoconf.h"
        ]
    withTempDir $ \tmpdir -> build $
        target context DeriveConstants [] [file, tmpdir]

copyRules :: Rules ()
copyRules = do
    root <- buildRootRules
    forM_ allStages $ \stage -> do
        let prefix = root -/- stageString stage -/- "lib"

            infixl 1 <~
            pattern <~ mdir = pattern %> \file -> do
                dir <- mdir
                copyFile (dir -/- makeRelative prefix file) file

        prefix -/- "ghc-usage.txt"     <~ return "driver"
        prefix -/- "ghci-usage.txt"    <~ return "driver"
        prefix -/- "llvm-targets"      <~ return "."
        prefix -/- "llvm-passes"       <~ return "."
        prefix -/- "ghc-interp.js"     <~ return "."
        prefix -/- "template-hsc.h" <~ return (pkgPath hsc2hs -/- "data")

        prefix -/- "post-link.mjs" %> \file -> do
            copyFile ("utils/jsffi" -/- makeRelative prefix file) file
            makeExecutable file

        prefix -/- "prelude.mjs"       <~ pure "utils/jsffi"

        prefix -/- "html/**"           <~ return "utils/haddock/haddock-api/resources"
        prefix -/- "latex/**"          <~ return "utils/haddock/haddock-api/resources"

        forM_ [Inplace, Final] $ \iplace ->
          root -/- relativePackageDbPath (PackageDbLoc stage iplace) -/- systemCxxStdLibConf %> \file -> do
            copyFile ("mk" -/- "system-cxx-std-lib-1.0.conf") file

generateRules :: Rules ()
generateRules = do
    root <- buildRootRules

    (root -/- "ghc-stage1") <~+ ghcWrapper Stage1
    (root -/- "ghc-stage2") <~+ ghcWrapper Stage2
    (root -/- "ghc-stage3") <~+ ghcWrapper Stage3

    forM_ allStages $ \stage -> do
        let prefix = root -/- stageString stage -/- "lib"
            go gen file = generate file (semiEmptyTarget (succStage stage)) gen
        (prefix -/- "settings") %> \out -> go (generateSettings out) out

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

-- | A set of interpolation variable substitutions.
newtype Interpolations = Interpolations (Action [(String, String)])

instance Semigroup Interpolations where
    Interpolations m <> Interpolations n = Interpolations ((++) <$> m <*> n)

instance Monoid Interpolations where
    mempty = Interpolations $ return []

-- | @interpolateVar var value@ is an interpolation which replaces @\@var\@@
-- with the result of @value@.
interpolateVar :: String -> Action String -> Interpolations
interpolateVar var value = Interpolations $ do
    val <- value
    return [(var, val)]

runInterpolations :: Interpolations -> String -> Action String
runInterpolations (Interpolations mk_substs) input = do
    substs <- mk_substs
    let subst :: String -> String
        subst = foldr (.) id [replace ("@"++k++"@") v | (k,v) <- substs]
    return (subst input)

-- | Interpolate the given variable with the value of the given 'Setting'.
interpolateSetting :: String -> Setting -> Interpolations
interpolateSetting name settng = interpolateVar name $ setting settng

-- | Interpolate the @ProjectVersion@, @ProjectVersionMunged@, and @ProjectVersionForLib@ variables.
projectVersion :: Interpolations
projectVersion = mconcat
    [ interpolateSetting "ProjectVersion" ProjectVersion
    , interpolateSetting "ProjectVersionMunged" ProjectVersionMunged
    , interpolateSetting "ProjectVersionForLib" ProjectVersionForLib
    ]

packageVersions :: Interpolations
packageVersions = foldMap f [ base, ghcPrim, compiler, ghc, cabal, templateHaskell, ghcCompact, array ]
  where
    f :: Package -> Interpolations
    f pkg = interpolateVar var $ version <$> readPackageData pkg
      where var = "LIBRARY_" <> escapedPkgName pkg <> "_VERSION"

packageUnitIds :: Stage -> Interpolations
packageUnitIds stage =
    foldMap f [ base, ghcPrim, compiler, ghc, cabal, templateHaskell, ghcCompact, array ]
  where
    f :: Package -> Interpolations
    f pkg = interpolateVar var $ pkgUnitId stage pkg
      where var = "LIBRARY_" <> escapedPkgName pkg <> "_UNIT_ID"

escapedPkgName :: Package -> String
escapedPkgName = map f . pkgName
  where
    f '-'   = '_'
    f other = other

templateRuleFrom :: FilePath -> FilePath -> Interpolations -> Rules ()
templateRuleFrom inPath outPath interps = do
    outPath %> \_ -> do
        s <- readFile' inPath
        result <- runInterpolations interps s
        writeFile' outPath result
        putSuccess ("| Successfully generated " ++ outPath ++ " from its template")

templateRule :: FilePath -> Interpolations -> Rules ()
templateRule outPath =
  templateRuleFrom (outPath <.> "in") outPath

templateRules :: Rules ()
templateRules = do
  templateRule "compiler/ghc.cabal" $ projectVersion
  templateRule "driver/ghci/ghci-wrapper.cabal" $ projectVersion
  templateRule "ghc/ghc-bin.cabal" $ projectVersion
  templateRule "utils/iserv/iserv.cabal" $ projectVersion
  templateRule "utils/remote-iserv/remote-iserv.cabal" $ projectVersion
  templateRule "utils/runghc/runghc.cabal" $ projectVersion
  templateRule "libraries/ghc-boot/ghc-boot.cabal" $ projectVersion
  templateRule "libraries/ghc-boot-th/ghc-boot-th.cabal" $ mconcat
    [ projectVersion
    , interpolateVar "Suffix" $ pure ""
    , interpolateVar "SourceRoot" $ pure "."
    ]
  templateRuleFrom "libraries/ghc-boot-th/ghc-boot-th.cabal.in"
                   "libraries/ghc-boot-th-next/ghc-boot-th-next.cabal" $ mconcat
    [ projectVersion
    , interpolateVar "Suffix" $ pure "-next"
    , interpolateVar "SourceRoot" $ pure "../ghc-boot-th"
    ]
  templateRule "libraries/ghci/ghci.cabal" $ projectVersion
  templateRule "libraries/ghc-heap/ghc-heap.cabal" $ projectVersion
  templateRule "libraries/ghc-internal/ghc-internal.cabal" $ projectVersion
  templateRule "libraries/ghc-experimental/ghc-experimental.cabal" $ projectVersion
  templateRule "libraries/base/base.cabal" $ projectVersion
  templateRule "utils/ghc-pkg/ghc-pkg.cabal" $ projectVersion
  templateRule "libraries/template-haskell/template-haskell.cabal" $ mconcat
    [ projectVersion
    ]
  templateRule "libraries/prologue.txt" $ packageVersions
  templateRule "rts/include/ghcversion.h" $ mconcat
    [ interpolateSetting "ProjectVersionInt" ProjectVersionInt
    , interpolateSetting "ProjectVersion" ProjectVersion
    , interpolateSetting "ProjectPatchLevel1" ProjectPatchLevel1
    , interpolateSetting "ProjectPatchLevel2" ProjectPatchLevel2
    ]
  templateRule "docs/index.html" $ packageUnitIds Stage1
  templateRule "docs/users_guide/ghc_config.py" $ mconcat
    [ projectVersion
    , packageUnitIds Stage1
    , interpolateSetting "LlvmMinVersion" LlvmMinVersion
    , interpolateSetting "LlvmMaxVersion" LlvmMaxVersion
    ]
  templateRule "compiler/GHC/CmmToLlvm/Version/Bounds.hs" $ mconcat
    [ interpolateVar "LlvmMinVersion" $ replaceEq '.' ',' <$> setting LlvmMinVersion
    , interpolateVar "LlvmMaxVersion" $ replaceEq '.' ',' <$> setting LlvmMaxVersion
    ]
  bindistRules

bindistRules :: Rules ()
bindistRules = do
  templateRule ("mk" -/- "project.mk") $ mconcat
    [ interpolateSetting "ProjectName" ProjectName
    , interpolateSetting "ProjectVersion" ProjectVersion
    , interpolateSetting "ProjectVersionInt" ProjectVersionInt
    , interpolateSetting "ProjectPatchLevel" ProjectPatchLevel
    , interpolateSetting "ProjectPatchLevel1" ProjectPatchLevel1
    , interpolateSetting "ProjectPatchLevel2" ProjectPatchLevel2
    , interpolateSetting "ProjectGitCommitId" ProjectGitCommitId

    , interpolateVar "HostOS_CPP" $ fmap cppify $ interp $ queryHost queryOS

    , interpolateVar "TargetPlatform" $ getTarget targetPlatformTriple
    , interpolateVar "TargetPlatform_CPP" $ cppify <$> getTarget targetPlatformTriple
    , interpolateVar "TargetArch_CPP" $ cppify <$> getTarget queryArch
    , interpolateVar "TargetOS_CPP" $ cppify <$> getTarget queryOS
    , interpolateVar "LLVMTarget" $ getTarget tgtLlvmTarget
    ]
  templateRule ("distrib" -/- "configure.ac") $ mconcat
    [ interpolateSetting "ConfiguredEmsdkVersion" EmsdkVersion
    , interpolateVar "CrossCompilePrefix" $ do
        crossCompiling <- interp $ getFlag CrossCompiling
        tpf <- setting TargetPlatformFull
        pure $ if crossCompiling then tpf <> "-" else ""
    , interpolateVar "LeadingUnderscore" $ yesNo <$> getTarget tgtSymbolsHaveLeadingUnderscore
    , interpolateSetting "LlvmMaxVersion" LlvmMaxVersion
    , interpolateSetting "LlvmMinVersion" LlvmMinVersion
    , interpolateVar "LlvmTarget" $ getTarget tgtLlvmTarget
    , interpolateSetting "ProjectVersion" ProjectVersion
    , interpolateVar "SettingsUseDistroMINGW" $ settingsFileSetting ToolchainSetting_DistroMinGW
    , interpolateVar "TablesNextToCode" $ yesNo <$> getTarget tgtTablesNextToCode
    , interpolateVar "TargetHasLibm" $ lookupSystemConfig "target-has-libm"
    , interpolateVar "TargetPlatform" $ getTarget targetPlatformTriple
    , interpolateVar "TargetWordBigEndian" $ getTarget isBigEndian
    , interpolateVar "TargetWordSize" $ getTarget wordSize
    , interpolateVar "Unregisterised" $ yesNo <$> getTarget tgtUnregisterised
    , interpolateVar "UseLibdw" $ fmap yesNo $ interp $ getFlag UseLibdw
    , interpolateVar "UseLibffiForAdjustors" $ yesNo <$> getTarget tgtUseLibffiForAdjustors
    , interpolateVar "GhcWithSMP" $ yesNo <$> targetSupportsSMP
    ]
  where
    interp = interpretInContext (semiEmptyTarget Stage2)
    getTarget = interp . queryTarget

-- | Given a 'String' replace characters '.' and '-' by underscores ('_') so that
-- the resulting 'String' is a valid C preprocessor identifier.
cppify :: String -> String
cppify = replaceEq '-' '_' . replaceEq '.' '_'


-- Generators

-- | GHC wrapper scripts used for passing the path to the right package database
-- when invoking in-tree GHC executables.
ghcWrapper :: Stage -> Expr String
ghcWrapper (Stage0 {}) = error "Stage0 GHC does not require a wrapper script to run."
ghcWrapper stage  = do
    ghcPath <- expr $ (</>) <$> topDirectory
                            <*> programPath (vanillaContext (predStage stage) ghc)
    return $ unwords $ map show $ [ ghcPath ]
                               ++ [ "$@" ]

generateSettings :: FilePath -> Expr String
generateSettings settingsFile = do
    ctx <- getContext
    stage <- getStage

    package_db_path <- expr $ do
      let get_pkg_db stg = packageDbPath (PackageDbLoc stg Final)
      case stage of
        Stage0 {} -> error "Unable to generate settings for stage0"
        Stage1 -> get_pkg_db Stage1
        Stage2 -> get_pkg_db Stage1
        Stage3 -> get_pkg_db Stage2

    let rel_pkg_db = makeRelativeNoSysLink (dropFileName settingsFile) package_db_path

    settings <- traverse sequence $
        [ ("C compiler command",   queryTarget ccPath)
        , ("C compiler flags",     queryTarget ccFlags)
        , ("C++ compiler command", queryTarget cxxPath)
        , ("C++ compiler flags",   queryTarget cxxFlags)
        , ("C compiler link flags",       queryTarget clinkFlags)
        , ("C compiler supports -no-pie", queryTarget linkSupportsNoPie)
        , ("CPP command",         queryTarget cppPath)
        , ("CPP flags",           queryTarget cppFlags)
        , ("Haskell CPP command", queryTarget hsCppPath)
        , ("Haskell CPP flags",   queryTarget hsCppFlags)
        , ("JavaScript CPP command", queryTarget jsCppPath)
        , ("JavaScript CPP flags", queryTarget jsCppFlags)
        , ("C-- CPP command", queryTarget cmmCppPath)
        , ("C-- CPP flags",   queryTarget cmmCppFlags)
        , ("C-- CPP supports -g0", queryTarget cmmCppSupportsG0')
        , ("ld supports compact unwind", queryTarget linkSupportsCompactUnwind)
        , ("ld supports filelist",       queryTarget linkSupportsFilelist)
        , ("ld supports single module",       queryTarget linkSupportsSingleModule)
        , ("ld is GNU ld",               queryTarget linkIsGnu)
        , ("Merge objects command", queryTarget mergeObjsPath)
        , ("Merge objects flags", queryTarget mergeObjsFlags)
        , ("Merge objects supports response files", queryTarget mergeObjsSupportsResponseFiles')
        , ("ar command",          queryTarget arPath)
        , ("ar flags",            queryTarget arFlags)
        , ("ar supports at file", queryTarget arSupportsAtFile')
        , ("ar supports -L",      queryTarget arSupportsDashL')
        , ("ranlib command", queryTarget ranlibPath)
        , ("otool command", expr $ settingsFileSetting ToolchainSetting_OtoolCommand)
        , ("install_name_tool command", expr $ settingsFileSetting ToolchainSetting_InstallNameToolCommand)
        , ("windres command", queryTarget (maybe "/bin/false" prgPath . tgtWindres)) -- TODO: /bin/false is not available on many distributions by default, but we keep it as it were before the ghc-toolchain patch. Fix-me.
        , ("unlit command", ("$topdir/../bin/" <>) <$> expr (programName (ctx { Context.package = unlit })))
        , ("cross compiling", expr $ yesNo <$> flag CrossCompiling)
        , ("target platform string", queryTarget targetPlatformTriple)
        , ("target os",        queryTarget (show . archOS_OS . tgtArchOs))
        , ("target arch",      queryTarget (show . archOS_arch . tgtArchOs))
        , ("target word size", queryTarget wordSize)
        , ("target word big endian",       queryTarget isBigEndian)
        , ("target has GNU nonexec stack", queryTarget (yesNo . Toolchain.tgtSupportsGnuNonexecStack))
        , ("target has .ident directive",  queryTarget (yesNo . Toolchain.tgtSupportsIdentDirective))
        , ("target has subsections via symbols", queryTarget (yesNo . Toolchain.tgtSupportsSubsectionsViaSymbols))
        , ("target has libm", expr $  lookupSystemConfig "target-has-libm")
        , ("Unregisterised", queryTarget (yesNo . tgtUnregisterised))
        , ("LLVM target", queryTarget tgtLlvmTarget)
        , ("LLVM llc command", expr $ settingsFileSetting ToolchainSetting_LlcCommand)
        , ("LLVM opt command", expr $ settingsFileSetting ToolchainSetting_OptCommand)
        , ("LLVM llvm-as command", expr $ settingsFileSetting ToolchainSetting_LlvmAsCommand)
        , ("Use inplace MinGW toolchain", expr $ settingsFileSetting ToolchainSetting_DistroMinGW)

        , ("Use interpreter", expr $ yesNo <$> ghcWithInterpreter)
        , ("Support SMP", expr $ yesNo <$> targetSupportsSMP)
        , ("RTS ways", escapeArgs . map show . Set.toList <$> getRtsWays)
        , ("Tables next to code", queryTarget (yesNo . tgtTablesNextToCode))
        , ("Leading underscore",  queryTarget (yesNo . tgtSymbolsHaveLeadingUnderscore))
        , ("Use LibFFI", expr $ yesNo <$> useLibffiForAdjustors)
        , ("RTS expects libdw", yesNo <$> getFlag UseLibdw)
        , ("Relative Global Package DB", pure rel_pkg_db)
        ]
    let showTuple (k, v) = "(" ++ show k ++ ", " ++ show v ++ ")"
    pure $ case settings of
        [] -> "[]"
        s : ss -> unlines $
            ("[" ++ showTuple s)
            : ((\s' -> "," ++ showTuple s') <$> ss)
            ++ ["]"]
  where
    ccPath  = prgPath . ccProgram . tgtCCompiler
    ccFlags = escapeArgs . prgFlags . ccProgram . tgtCCompiler
    cxxPath  = prgPath . cxxProgram . tgtCxxCompiler
    cxxFlags = escapeArgs . prgFlags . cxxProgram . tgtCxxCompiler
    clinkFlags = escapeArgs . prgFlags . ccLinkProgram . tgtCCompilerLink
    linkSupportsNoPie = yesNo . ccLinkSupportsNoPie . tgtCCompilerLink
    cppPath  = prgPath . cppProgram . tgtCPreprocessor
    cppFlags = escapeArgs . prgFlags . cppProgram . tgtCPreprocessor
    hsCppPath  = prgPath . hsCppProgram . tgtHsCPreprocessor
    hsCppFlags = escapeArgs . prgFlags . hsCppProgram . tgtHsCPreprocessor
    jsCppPath  = maybe "" (prgPath . jsCppProgram) . tgtJsCPreprocessor
    jsCppFlags = maybe "" (escapeArgs . prgFlags . jsCppProgram) . tgtJsCPreprocessor
    cmmCppPath  = prgPath . cmmCppProgram . tgtCmmCPreprocessor
    cmmCppFlags = escapeArgs . prgFlags . cmmCppProgram . tgtCmmCPreprocessor
    cmmCppSupportsG0' = yesNo . cmmCppSupportsG0 . tgtCmmCPreprocessor
    mergeObjsPath  = maybe "" (prgPath . mergeObjsProgram) . tgtMergeObjs
    mergeObjsFlags = maybe "" (escapeArgs . prgFlags . mergeObjsProgram) . tgtMergeObjs
    linkSupportsSingleModule    = yesNo . ccLinkSupportsSingleModule . tgtCCompilerLink
    linkSupportsFilelist        = yesNo . ccLinkSupportsFilelist . tgtCCompilerLink
    linkSupportsCompactUnwind   = yesNo . ccLinkSupportsCompactUnwind . tgtCCompilerLink
    linkIsGnu                   = yesNo . ccLinkIsGnu . tgtCCompilerLink
    arPath  = prgPath . arMkArchive . tgtAr
    arFlags = escapeArgs . prgFlags . arMkArchive . tgtAr
    arSupportsAtFile' = yesNo . arSupportsAtFile . tgtAr
    arSupportsDashL' = yesNo . arSupportsDashL . tgtAr
    ranlibPath  = maybe "" (prgPath . ranlibProgram) . tgtRanlib
    mergeObjsSupportsResponseFiles' = maybe "NO" (yesNo . mergeObjsSupportsResponseFiles) . tgtMergeObjs

isBigEndian, wordSize :: Toolchain.Target -> String
isBigEndian = yesNo . (\case BigEndian -> True; LittleEndian -> False) . tgtEndianness
wordSize    = show . wordSize2Bytes . tgtWordSize

-- | Generate @Config.hs@ files.
generateConfigHs :: Expr String
generateConfigHs = do
    stage <- getStage
    let chooseSetting x y = case stage of { Stage0 {} -> x; _ -> y }
    buildPlatform <- chooseSetting (queryBuild targetPlatformTriple) (queryHost targetPlatformTriple)
    hostPlatform <- chooseSetting (queryHost targetPlatformTriple) (queryTarget targetPlatformTriple)
    trackGenerateHs
    cProjectName        <- getSetting ProjectName
    cBooterVersion      <- getSetting GhcVersion
    -- We now give a unit-id with a version and a hash to ghc.
    -- See Note [GHC's Unit Id] in GHC.Unit.Types
    --
    -- It's crucial that the unit-id matches the unit-key -- ghc is no longer
    -- part of the WiringMap, so we don't to go back and forth between the
    -- unit-id and the unit-key -- we take care that they are the same by using
    -- 'pkgUnitId' on 'compiler' (the ghc-library package) to create the
    -- unit-id in both situations.
    cProjectUnitId <- expr . (`pkgUnitId` compiler) =<< getStage
    return $ unlines
        [ "module GHC.Settings.Config"
        , "  ( module GHC.Version"
        , "  , cBuildPlatformString"
        , "  , cHostPlatformString"
        , "  , cProjectName"
        , "  , cBooterVersion"
        , "  , cStage"
        , "  , cProjectUnitId"
        , "  ) where"
        , ""
        , "import GHC.Prelude.Basic"
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
        , "cStage                = show (" ++ stageString stage ++ " :: Int)"
        , ""
        , "cProjectUnitId :: String"
        , "cProjectUnitId = " ++ show cProjectUnitId
        ]
  where
    stageString (Stage0 InTreeLibs) = "1"
    stageString Stage1 = "2"
    stageString Stage2 = "3"
    stageString Stage3 = "4"
    stageString (Stage0 GlobalLibs) = error "stageString: StageBoot"


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
    cHostPlatformArch <- queryHost (archOS_arch . tgtArchOs)
    cHostPlatformOS   <- queryHost (archOS_OS . tgtArchOs)
    return $ unlines
        [ "module GHC.Platform.Host where"
        , ""
        , "import GHC.Platform.ArchOS"
        , ""
        , "hostPlatformArch :: Arch"
        , "hostPlatformArch = " ++ show cHostPlatformArch
        , ""
        , "hostPlatformOS   :: OS"
        , "hostPlatformOS   = " ++ show cHostPlatformOS
        , ""
        , "hostPlatformArchOS :: ArchOS"
        , "hostPlatformArchOS = ArchOS hostPlatformArch hostPlatformOS"
        ]

-- | Just like 'GHC.ResponseFile.escapeArgs', but use spaces instead of newlines
-- for splitting elements.
escapeArgs :: [String] -> String
escapeArgs = unwords . map escapeArg

escapeArg :: String -> String
escapeArg = reverse . foldl' escape []

escape :: String -> Char -> String
escape cs c
  |    isSpace c
    || '\\' == c
    || '\'' == c
    || '"'  == c = c:'\\':cs -- n.b., our caller must reverse the result
  | otherwise    = c:cs
