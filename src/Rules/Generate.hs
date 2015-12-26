module Rules.Generate (generatePackageCode) where

import Expression
import GHC
import Rules.Generators.ConfigHs
import Rules.Generators.VersionHs
import Rules.Generators.PlatformH
import Oracles.ModuleFiles
import Rules.Actions
import Rules.Resources (Resources)
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
        buildPath   = path -/- "build"
        primopsTxt  = targetPath stage compiler -/- "build/primops.txt"
        platformH   = targetPath stage compiler -/- "ghc_boot_platform.h"
        generated f = (buildPath ++ "//*.hs") ?== f && not ("//autogen/*" ?== f)
        generate file expr = do
            contents <- interpretPartial target expr
            writeFileChanged file contents
            putBuild $ "| Successfully generated '" ++ file ++ "'."
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

        priority 2.0 $ do
            when (pkg == ghcPkg) $ buildPath -/- "Config.hs" %> \file -> do
                generate file generateConfigHs

            when (pkg == ghcPkg) $ buildPath -/- "Version.hs" %> \file -> do
                generate file generateVersionHs

            when (pkg == compiler) $ platformH %> \file -> do
                generate file generatePlatformH

            when (pkg == runghc) $ buildPath -/- "Main.hs" %> \file -> do
                copyFileChanged (pkgPath pkg -/- "runghc.hs") file
                putBuild $ "| Successfully generated '" ++ file ++ "'."
