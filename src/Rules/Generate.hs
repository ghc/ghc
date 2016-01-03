module Rules.Generate (
    generatePackageCode, generateRules,
    derivedConstantsPath, generatedDependencies
    ) where

import Base
import Expression
import GHC
import Rules.Generators.ConfigHs
import Rules.Generators.GhcAutoconfH
import Rules.Generators.GhcBootPlatformH
import Rules.Generators.GhcPlatformH
import Rules.Generators.GhcVersionH
import Rules.Generators.VersionHs
import Oracles.ModuleFiles
import Rules.Actions
import Rules.Resources (Resources)
import Settings

primopsSource :: FilePath
primopsSource = "compiler/prelude/primops.txt.pp"

primopsTxt :: Stage -> FilePath
primopsTxt stage = targetPath stage compiler -/- "build/primops.txt"

platformH :: Stage -> FilePath
platformH stage = targetPath stage compiler -/- "ghc_boot_platform.h"

derivedConstantsPath :: FilePath
derivedConstantsPath = "includes/dist-derivedconstants/header"

-- TODO: can we drop COMPILER_INCLUDES_DEPS += $(includes_GHCCONSTANTS)?
generatedDependencies :: Stage -> Package -> [FilePath]
generatedDependencies stage pkg
    | pkg == compiler = let buildPath = targetPath stage compiler -/- "build"
                        in
                        [ "includes/ghcautoconf.h"
                        , "includes/ghcplatform.h"
                        , derivedConstantsPath -/- "DerivedConstants.h"
                        , derivedConstantsPath -/- "GHCConstantsHaskellType.hs"
                        , derivedConstantsPath -/- "GHCConstantsHaskellWrappers.hs"
                        , derivedConstantsPath -/- "GHCConstantsHaskellExports.hs"
                        , platformH stage ]
                        ++
                        fmap (buildPath -/-)
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
    | pkg == hp2ps    = [ "includes/ghcautoconf.h"
                        , "includes/ghcplatform.h" ]
    | pkg == rts      = let buildPath = targetPath stage rts -/- "build"
                        in
                        [ "includes/ghcversion.h" -- missing only in stage1. See #76
                        , derivedConstantsPath -/- "DerivedConstants.h" ]
                        ++
                        fmap (buildPath -/-) ["ffi.h", "ffitarget.h"]
    | otherwise = []

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
                copyFileChanged srcBoot $ file -<.> "hs-boot"

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
            when (pkg == compiler && stage == Stage1) $
                derivedConstantsPath ++ "//*" %> \file -> do
                    build $ fullTarget target DeriveConstants [] [file]

            when (pkg == compiler) $ buildPath -/- "Config.hs" %> \file -> do
                file <~ generateConfigHs

            when (pkg == compiler) $ platformH stage %> \file -> do
                file <~ generateGhcBootPlatformH

            when (pkg == ghcPkg) $ buildPath -/- "Version.hs" %> \file -> do
                file <~ generateVersionHs

            when (pkg == runGhc) $ buildPath -/- "Main.hs" %> \file -> do
                copyFileChanged (pkgPath pkg -/- "runghc.hs") file
                putSuccess $ "| Successfully generated '" ++ file ++ "'."

generateRules :: Rules ()
generateRules = do
    "includes/ghcautoconf.h" <~ generateGhcAutoconfH
    "includes/ghcplatform.h" <~ generateGhcPlatformH
    "includes/ghcversion.h"  <~ generateGhcVersionH
  where
    file <~ gen = file %> \out -> generate out emptyTarget gen

-- TODO: Use the Types, Luke! (drop partial function)
-- We sometimes need to evaluate expressions that do not require knowing all
-- information about the target. In this case, we don't want to know anything.
emptyTarget :: PartialTarget
emptyTarget = PartialTarget (error "Rules.Generate.emptyTarget: unknown stage")
                            (error "Rules.Generate.emptyTarget: unknown package")
