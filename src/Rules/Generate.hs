module Rules.Generate (
    generatePackageCode, generateRules, installTargets, copyRules,
    derivedConstantsPath, generatedDependencies
    ) where

import Base
import Expression
import GHC
import Rules.Generators.ConfigHs
import Rules.Generators.GhcAutoconfH
import Rules.Generators.GhcBootPlatformH
import Rules.Generators.GhcPlatformH
import Rules.Generators.GhcSplit
import Rules.Generators.GhcVersionH
import Rules.Generators.VersionHs
import Oracles.ModuleFiles
import Rules.Actions
import Rules.Gmp
import Rules.Libffi
import Rules.Resources (Resources)
import Settings

installTargets :: [FilePath]
installTargets = [ "inplace/lib/template-hsc.h"
                 , "inplace/lib/platformConstants"
                 , "inplace/lib/settings" ]

primopsSource :: FilePath
primopsSource = "compiler/prelude/primops.txt.pp"

primopsTxt :: Stage -> FilePath
primopsTxt stage = targetPath stage compiler -/- "build/primops.txt"

platformH :: Stage -> FilePath
platformH stage = targetPath stage compiler -/- "ghc_boot_platform.h"

-- TODO: move generated files to buildRootPath, see #113
includesDependencies :: [FilePath]
includesDependencies = ("includes" -/-) <$>
    [ "ghcautoconf.h"
    , "ghcplatform.h"
    , "ghcversion.h" ]

defaultDependencies :: [FilePath]
defaultDependencies = concat
    [ includesDependencies
    , libffiDependencies
    , gmpDependencies ]

ghcPrimDependencies :: Stage -> [FilePath]
ghcPrimDependencies stage = ((targetPath stage ghcPrim -/- "build") -/-) <$>
       [ "GHC/PrimopWrappers.hs"
       , "autogen/GHC/Prim.hs" ]

derivedConstantsPath :: FilePath
derivedConstantsPath = "includes/dist-derivedconstants/header"

derivedConstantsDependencies :: [FilePath]
derivedConstantsDependencies = installTargets ++ fmap (derivedConstantsPath -/-)
    [ "DerivedConstants.h"
    , "GHCConstantsHaskellType.hs"
    , "GHCConstantsHaskellWrappers.hs"
    , "GHCConstantsHaskellExports.hs" ]

compilerDependencies :: Stage -> [FilePath]
compilerDependencies stage =
    [ platformH stage ]
    ++ defaultDependencies ++ derivedConstantsDependencies
    ++ fmap ((targetPath stage compiler -/- "build") -/-)
       [ "primop-vector-uniques.hs-incl"
       , "primop-data-decl.hs-incl"
       , "primop-tag.hs-incl"
       , "primop-list.hs-incl"
       , "primop-strictness.hs-incl"
       , "primop-fixity.hs-incl"
       , "primop-primop-info.hs-incl"
       , "primop-out-of-line.hs-incl"
       , "primop-has-side-effects.hs-incl"
       , "primop-can-fail.hs-incl"
       , "primop-code-size.hs-incl"
       , "primop-commutable.hs-incl"
       , "primop-vector-tys-exports.hs-incl"
       , "primop-vector-tycons.hs-incl"
       , "primop-vector-tys.hs-incl" ]

generatedDependencies :: Stage -> Package -> [FilePath]
generatedDependencies stage pkg
    | pkg   == compiler = compilerDependencies stage
    | pkg   == ghcPrim  = ghcPrimDependencies stage
    | pkg   == rts      = libffiDependencies ++ includesDependencies
                       ++ derivedConstantsDependencies
    | stage == Stage0   = defaultDependencies
    | otherwise         = []

-- The following generators and corresponding source extensions are supported:
knownGenerators :: [ (Builder, String) ]
knownGenerators =  [ (Alex  , ".x"  )
                   , (Happy , ".y"  )
                   , (Happy , ".ly" )
                   , (Hsc2Hs, ".hsc") ]

determineBuilder :: FilePath -> Maybe Builder
determineBuilder file = fmap fst $ find (\(_, e) -> e == ext) knownGenerators
  where
    ext = takeExtension file

generate :: FilePath -> PartialTarget -> Expr String -> Action ()
generate file target expr = do
    contents <- interpretPartial target expr
    writeFileChanged file contents
    putSuccess $ "| Successfully generated '" ++ file ++ "'."

generatePackageCode :: Resources -> PartialTarget -> Rules ()
generatePackageCode _ target @ (PartialTarget stage pkg) =
    let buildPath   = targetPath stage pkg -/- "build"
        dropBuild   = drop (length buildPath + 1)
        generated f = (buildPath ++ "//*.hs") ?== f && not ("//autogen/*" ?== f)
        file <~ gen = generate file target gen
    in do
        generated ?> \file -> do
            let srcFile = dropBuild file
                pattern = "//" ++ srcFile -<.> "*"
            files <- fmap (filter (pattern ?==)) $ moduleFiles stage pkg
            let gens = [ (f, b) | f <- files, Just b <- [determineBuilder f] ]
            when (length gens /= 1) . putError $
                "Exactly one generator expected for " ++ file
                ++ " (found: " ++ show gens ++ ")."
            let (src, builder) = head gens
            need [src]
            build $ fullTarget target builder [src] [file]
            let srcBoot = src -<.> "hs-boot"
            whenM (doesFileExist srcBoot) $
                copyFile srcBoot $ file -<.> "hs-boot"

        -- TODO: needing platformH is ugly and fragile
        when (pkg == compiler) $ primopsTxt stage %> \file -> do
            need [platformH stage, primopsSource]
            build $ fullTarget target HsCpp [primopsSource] [file]

        -- TODO: why different folders for generated files?
        fmap (buildPath -/-)
            [ "GHC/PrimopWrappers.hs"
            , "autogen/GHC/Prim.hs"
            , "*.hs-incl" ] |%> \file -> do
                need [primopsTxt stage]
                build $ fullTarget target GenPrimopCode [primopsTxt stage] [file]

        when (pkg == rts) $ buildPath -/- "AutoApply.cmm" %> \file -> do
            build $ fullTarget target GenApply [] [file]

        priority 2.0 $ do
            when (pkg == compiler) $ buildPath -/- "Config.hs" %> \file -> do
                file <~ generateConfigHs

            when (pkg == compiler) $ platformH stage %> \file -> do
                file <~ generateGhcBootPlatformH

            when (pkg == ghcPkg) $ buildPath -/- "Version.hs" %> \file -> do
                file <~ generateVersionHs

            when (pkg == runGhc) $ buildPath -/- "Main.hs" %> \file -> do
                copyFileChanged (pkgPath pkg -/- "runghc.hs") file
                putSuccess $ "| Successfully generated '" ++ file ++ "'."

copyRules :: Rules ()
copyRules = do
    "inplace/lib/template-hsc.h"    <~ pkgPath hsc2hs
    "inplace/lib/platformConstants" <~ derivedConstantsPath
    "inplace/lib/settings"          <~ "."
  where
    file <~ dir = file %> \_ -> copyFile (dir -/- takeFileName file) file

generateRules :: Rules ()
generateRules = do
    "includes/ghcautoconf.h" <~ generateGhcAutoconfH
    "includes/ghcplatform.h" <~ generateGhcPlatformH
    "includes/ghcversion.h"  <~ generateGhcVersionH

    ghcSplit %> \_ -> do
        generate ghcSplit emptyTarget generateGhcSplit
        makeExecutable ghcSplit

    -- TODO: simplify, get rid of fake rts target
    derivedConstantsPath ++ "//*" %> \file -> do
        withTempDir $ \dir -> build $
            fullTarget (PartialTarget Stage1 rts) DeriveConstants [] [file, dir]

  where
    file <~ gen = file %> \out -> generate out emptyTarget gen

-- TODO: Use the Types, Luke! (drop partial function)
-- We sometimes need to evaluate expressions that do not require knowing all
-- information about the target. In this case, we don't want to know anything.
emptyTarget :: PartialTarget
emptyTarget = PartialTarget (error "Rules.Generate.emptyTarget: unknown stage")
                            (error "Rules.Generate.emptyTarget: unknown package")
