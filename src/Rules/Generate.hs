module Rules.Generate (
    generatePackageCode, generateRules, installTargets, copyRules,
    includesDependencies, derivedConstantsPath, generatedDependencies
    ) where

import qualified System.Directory as IO

import Base
import Context
import Expression
import GHC
import Oracles.ModuleFiles
import Rules.Actions
import Rules.Generators.ConfigHs
import Rules.Generators.GhcAutoconfH
import Rules.Generators.GhcBootPlatformH
import Rules.Generators.GhcPlatformH
import Rules.Generators.GhcSplit
import Rules.Generators.GhcVersionH
import Rules.Generators.VersionHs
import Rules.Libffi
import Settings
import Target

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

-- TODO: move generated files to buildRootPath, see #113
includesDependencies :: [FilePath]
includesDependencies = ("includes" -/-) <$>
    [ "ghcautoconf.h"
    , "ghcplatform.h"
    , "ghcversion.h" ]

ghcPrimDependencies :: Stage -> [FilePath]
ghcPrimDependencies stage = (buildPath (vanillaContext stage ghcPrim) -/-) <$>
       [ "autogen/GHC/Prim.hs"
       , "GHC/PrimopWrappers.hs" ]

derivedConstantsPath :: FilePath
derivedConstantsPath = "includes/dist-derivedconstants/header"

derivedConstantsDependencies :: [FilePath]
derivedConstantsDependencies = installTargets ++ fmap (derivedConstantsPath -/-)
    [ "DerivedConstants.h"
    , "GHCConstantsHaskellExports.hs"
    , "GHCConstantsHaskellType.hs"
    , "GHCConstantsHaskellWrappers.hs" ]

compilerDependencies :: Stage -> [FilePath]
compilerDependencies stage =
    [ platformH stage ]
    ++ includesDependencies
    ++ [ gmpLibraryH | stage > Stage0 ]
    ++ filter (const $ stage > Stage0) libffiDependencies
    ++ derivedConstantsDependencies
    ++ fmap (buildPath (vanillaContext stage compiler) -/-)
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
       , "primop-vector-uniques.hs-incl" ]

-- TODO: Turn this into a FilePaths expression
generatedDependencies :: Stage -> Package -> [FilePath]
generatedDependencies stage pkg
    | pkg   == compiler = compilerDependencies stage
    | pkg   == ghcPrim  = ghcPrimDependencies stage
    | pkg   == rts      = libffiDependencies ++ includesDependencies
                       ++ derivedConstantsDependencies
    | stage == Stage0   = includesDependencies
    | otherwise         = []

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
            maybeValue <- findGenerator context file
            (src, builder) <- case maybeValue of
                Nothing    -> error $ "No generator for " ++ file ++ "."
                Just value -> return value
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
    "inplace/lib/platformConstants"  <~ derivedConstantsPath
    "inplace/lib/settings"           <~ "."
    "inplace/lib/template-hsc.h"     <~ pkgPath hsc2hs
    rtsBuildPath -/- "sm/Evac_thr.c" %> copyFile (pkgPath rts -/- "sm/Evac.c")
    rtsBuildPath -/- "sm/Scav_thr.c" %> copyFile (pkgPath rts -/- "sm/Scav.c")
  where
    file <~ dir = file %> copyFile (dir -/- takeFileName file)

generateRules :: Rules ()
generateRules = do
    "includes/ghcautoconf.h" <~ generateGhcAutoconfH
    "includes/ghcplatform.h" <~ generateGhcPlatformH
    "includes/ghcversion.h"  <~ generateGhcVersionH

    ghcSplit %> \_ -> do
        generate ghcSplit emptyTarget generateGhcSplit
        makeExecutable ghcSplit

    -- TODO: simplify, get rid of fake rts context
    derivedConstantsPath ++ "//*" %> \file -> do
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
