module Rules.Generate (
    generatePackageCode, generateRules, installTargets, copyRules,
    includesDependencies, generatedDependencies, getPathIfGenerated
    ) where

import qualified System.Directory as IO

import Base
import Context hiding (package)
import Expression
import GHC
import Oracles.ModuleFiles
import Predicate
import Rules.Actions
import Rules.Generators.ConfigHs
import Rules.Generators.GhcAutoconfH
import Rules.Generators.GhcBootPlatformH
import Rules.Generators.GhcPlatformH
import Rules.Generators.GhcSplit
import Rules.Generators.GhcVersionH
import Rules.Generators.VersionHs
import Rules.Libffi
import Settings.Paths
import Target
import UserSettings

installTargets :: [FilePath]
installTargets = [ "inplace/lib/ghc-usage.txt"
                 , "inplace/lib/ghci-usage.txt"
                 , "inplace/lib/platformConstants"
                 , "inplace/lib/settings"
                 , "inplace/lib/template-hsc.h" ]

primopsSource :: FilePath
primopsSource = "compiler/prelude/primops.txt.pp"

primopsTxt :: Stage -> FilePath
primopsTxt stage = buildPath (vanillaContext stage compiler) -/- "primops.txt"

platformH :: Stage -> FilePath
platformH stage = buildPath (vanillaContext stage compiler) -/- "ghc_boot_platform.h"

includesDependencies :: [FilePath]
includesDependencies = fmap (generatedPath -/-)
    [ "ghcautoconf.h"
    , "ghcplatform.h"
    , "ghcversion.h" ]
    ++ -- TODO: This is a temporary fix, see #113:
    [ "includes/ghcversion.h"]

ghcPrimDependencies :: Expr [FilePath]
ghcPrimDependencies = do
    stage <- getStage
    let path = buildPath $ vanillaContext stage ghcPrim
    return [path -/- "autogen/GHC/Prim.hs", path -/- "GHC/PrimopWrappers.hs"]

derivedConstantsDependencies :: [FilePath]
derivedConstantsDependencies = installTargets ++ fmap (generatedPath -/-)
    [ "DerivedConstants.h"
    , "GHCConstantsHaskellExports.hs"
    , "GHCConstantsHaskellType.hs"
    , "GHCConstantsHaskellWrappers.hs" ]

compilerDependencies :: Expr [FilePath]
compilerDependencies = do
    stage <- getStage
    let path = buildPath $ vanillaContext stage compiler
    mconcat [ return [platformH stage]
            , return includesDependencies
            , return derivedConstantsDependencies
            , notStage0 ? return (gmpLibraryH : libffiDependencies)
            , return $ fmap (path -/-)
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
generatedDependencies = mconcat
    [ package compiler ? compilerDependencies
    , package ghcPrim  ? ghcPrimDependencies
    , package rts      ? return (libffiDependencies
        ++ includesDependencies
        ++ derivedConstantsDependencies)
    , stage0 ? return includesDependencies ]

generate :: FilePath -> Context -> Expr String -> Action ()
generate file context expr = do
    contents <- interpretInContext context expr
    writeFileChanged file contents
    putSuccess $ "| Successfully generated " ++ file ++ "."

generatePackageCode :: Context -> Rules ()
generatePackageCode context@(Context stage pkg _) =
    let path        = buildPath context
        generated f = (path ++ "//*.hs") ?== f && not ("//autogen/*" ?== f)
        file <~ gen = generate file context gen
    in do
        generated ?> \file -> do
            let unpack = fromMaybe . error $ "No generator for " ++ file ++ "."
            (src, builder) <- unpack <$> findGenerator context file
            need [src]
            build $ Target context builder [src] [file]
            let srcBoot = src -<.> "hs-boot"
            whenM (doesFileExist srcBoot) $
                copyFile srcBoot $ file -<.> "hs-boot"

        -- TODO: needing platformH is ugly and fragile
        when (pkg == compiler) $ primopsTxt stage %> \file -> do
            need $ [platformH stage, primopsSource] ++ includesDependencies
            build $ Target context HsCpp [primopsSource] [file]

        -- TODO: why different folders for generated files?
        fmap (path -/-)
            [ "autogen/GHC/Prim.hs"
            , "GHC/PrimopWrappers.hs"
            , "*.hs-incl" ] |%> \file -> do
                need [primopsTxt stage]
                build $ Target context GenPrimopCode [primopsTxt stage] [file]
                -- TODO: this is temporary hack, get rid of this (#113)
                let oldPath = pkgPath pkg -/- contextDirectory context -/- "build"
                    newFile = oldPath ++ (drop (length path) file)
                createDirectory $ takeDirectory newFile
                liftIO $ IO.copyFile file newFile
                putBuild $ "| Duplicate file " ++ file ++ " -> " ++ newFile

        when (pkg == rts) $ path -/- "AutoApply.cmm" %> \file -> do
            build $ Target context GenApply [] [file]

        priority 2.0 $ do
            -- TODO: this is temporary hack, get rid of this (#113)
            let oldPath = pkgPath pkg -/- contextDirectory context -/- "build"
                olden f = oldPath ++ (drop (length (buildPath context)) f)

            when (pkg == compiler) $ path -/- "Config.hs" %> \file -> do
                file <~ generateConfigHs
                olden file <~ generateConfigHs -- TODO: get rid of this (#113)

            when (pkg == compiler) $ platformH stage %> \file -> do
                file <~ generateGhcBootPlatformH

            when (pkg == ghcPkg) $ path -/- "Version.hs" %> \file -> do
                file <~ generateVersionHs
                olden file <~ generateVersionHs -- TODO: get rid of this (#113)

            when (pkg == runGhc) $ path -/- "Main.hs" %> \file -> do
                copyFileChanged (pkgPath pkg -/- "runghc.hs") file
                putSuccess $ "| Successfully generated " ++ file ++ "."

copyRules :: Rules ()
copyRules = do
    "inplace/lib/ghc-usage.txt"      <~ "driver"
    "inplace/lib/ghci-usage.txt"     <~ "driver"
    "inplace/lib/platformConstants"  <~ generatedPath
    "inplace/lib/settings"           <~ "."
    "inplace/lib/template-hsc.h"     <~ pkgPath hsc2hs
    rtsBuildPath -/- "sm/Evac_thr.c" %> copyFile (pkgPath rts -/- "sm/Evac.c")
    rtsBuildPath -/- "sm/Scav_thr.c" %> copyFile (pkgPath rts -/- "sm/Scav.c")
    -- TODO: This is a temporary fix, see #113:
    "includes/ghcversion.h"          <~ generatedPath
  where
    file <~ dir = file %> copyFile (dir -/- takeFileName file)

generateRules :: Rules ()
generateRules = do
    (generatedPath -/- "ghcautoconf.h") <~ generateGhcAutoconfH
    (generatedPath -/- "ghcplatform.h") <~ generateGhcPlatformH
    (generatedPath -/-  "ghcversion.h") <~ generateGhcVersionH

    ghcSplit %> \_ -> do
        generate ghcSplit emptyTarget generateGhcSplit
        makeExecutable ghcSplit

    -- TODO: simplify, get rid of fake rts context
    generatedPath ++ "//*" %> \file -> do
        withTempDir $ \dir -> build $
            Target rtsContext DeriveConstants [] [file, dir]

  where
    file <~ gen = file %> \out -> generate out emptyTarget gen

-- TODO: Use the Types, Luke! (drop partial function)
-- We sometimes need to evaluate expressions that do not require knowing all
-- information about the context. In this case, we don't want to know anything.
emptyTarget :: Context
emptyTarget = vanillaContext (error "Rules.Generate.emptyTarget: unknown stage")
                             (error "Rules.Generate.emptyTarget: unknown package")

getPathIfGenerated :: FilePath -> Expr (Maybe FilePath)
getPathIfGenerated include = do
  generated <- generatedFiles
  -- For includes of generated files, we cannot get the full path of the file
  -- (since it might be included due to some include dir, i.e., through `-I`).
  -- So here we try both the name and the path.
  let nameOrPath (name, path) = include == name || include == path
  return . fmap snd $ find nameOrPath generated

generatedFiles :: Expr [(FilePath, FilePath)]
generatedFiles = do
  deps <- generatedDependencies
  return [ (takeFileName fp, fp) | fp <- deps ]
