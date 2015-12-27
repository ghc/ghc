module Rules.Generate (
    generatePackageCode, generateRules, includesDependencies
    ) where

import Expression
import GHC
import Rules.Generators.ConfigHs
import Rules.Generators.GhcAutoconfH
import Rules.Generators.GhcBootPlatformH
import Rules.Generators.GhcPlatformH
import Rules.Generators.VersionHs
import Oracles.ModuleFiles
import Rules.Actions
import Rules.Resources (Resources)
import Settings

primopsSource :: FilePath
primopsSource = "compiler/prelude/primops.txt.pp"

derivedConstantsPath :: FilePath
derivedConstantsPath = "includes/dist-derivedconstants/header"

-- TODO: can we drop COMPILER_INCLUDES_DEPS += $(includes_GHCCONSTANTS)?
includesDependencies :: [FilePath]
includesDependencies =
    [ "includes/ghcautoconf.h"
    , "includes/ghcplatform.h"
    , derivedConstantsPath -/- "DerivedConstants.h"
    , derivedConstantsPath -/- "GHCConstantsHaskellType.hs"
    , derivedConstantsPath -/- "GHCConstantsHaskellWrappers.hs"
    , derivedConstantsPath -/- "GHCConstantsHaskellExports.hs" ]

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

generate :: FilePath -> PartialTarget -> Expr String -> Action ()
generate file target expr = do
    contents <- interpretPartial target expr
    writeFileChanged file contents
    putSuccess $ "| Successfully generated '" ++ file ++ "'."

generatePackageCode :: Resources -> PartialTarget -> Rules ()
generatePackageCode _ target @ (PartialTarget stage pkg) =
    let path        = targetPath stage pkg
        buildPath   = path -/- "build"
        primopsTxt  = targetPath stage compiler -/- "build/primops.txt"
        platformH   = targetPath stage compiler -/- "ghc_boot_platform.h"
        generated f = (buildPath ++ "//*.hs") ?== f && not ("//autogen/*" ?== f)
        file <~ gen = generate file target gen
    in do
        generated ?> \file -> do
            let pattern = "//" ++ takeBaseName file <.> "*"
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
        when (pkg == compiler) $ primopsTxt %> \file -> do
            need [platformH, primopsSource]
            build $ fullTarget target HsCpp [primopsSource] [file]

        -- TODO: why different folders for generated files?
        fmap (buildPath -/-)
            [ "GHC/PrimopWrappers.hs"
            , "autogen/GHC/Prim.hs"
            , "*.hs-incl" ] |%> \file -> do
                need [primopsTxt]
                build $ fullTarget target GenPrimopCode [primopsTxt] [file]

        priority 2.0 $ do
            when (pkg == compiler && stage == Stage1) $
                derivedConstantsPath ++ "//*" %> \file -> do
                    build $ fullTarget target DeriveConstants [] [file]

            when (pkg == compiler) $ buildPath -/- "Config.hs" %> \file -> do
                file <~ generateConfigHs

            when (pkg == compiler) $ platformH %> \file -> do
                file <~ generateGhcBootPlatformH

            when (pkg == ghcPkg) $ buildPath -/- "Version.hs" %> \file -> do
                file <~ generateVersionHs

            when (pkg == runghc) $ buildPath -/- "Main.hs" %> \file -> do
                copyFileChanged (pkgPath pkg -/- "runghc.hs") file
                putSuccess $ "| Successfully generated '" ++ file ++ "'."

generateRules :: Rules ()
generateRules = do
    "includes/ghcautoconf.h" <~ generateGhcAutoconfH
    "includes/ghcplatform.h" <~ generateGhcPlatformH
  where
    file <~ gen = file %> \out -> generate out emptyTarget gen

-- TODO: Use the Types, Luke! (drop partial function)
-- We sometimes need to evaluate expressions that do not require knowing all
-- information about the target. In this case, we don't want to know anything.
emptyTarget :: PartialTarget
emptyTarget = PartialTarget (error "Rules.Generate.emptyTarget: unknown stage")
                            (error "Rules.Generate.emptyTarget: unknown package")
