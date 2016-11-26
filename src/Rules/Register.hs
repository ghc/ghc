module Rules.Register (registerPackage) where

import Base
import Context
import Expression
import GHC
import Oracles.Path
import Rules.Libffi
import Settings.Packages.Rts
import Settings.Path
import Target
import UserSettings
import Util

-- | Build rules for registering packages and initialising package databases
-- by running the @ghc-pkg@ utility.
registerPackage :: [(Resource, Int)] -> Context -> Rules ()
registerPackage rs context@Context {..} = when (stage <= Stage1) $ do
    let dir = packageDbDirectory stage

    matchVersionedFilePath (dir -/- pkgNameString package) "conf" ?> \conf -> do
        -- This produces inplace-pkg-config. TODO: Add explicit tracking.
        need [pkgDataFile context]

        -- Post-process inplace-pkg-config. TODO: remove, see #113, #148.
        top <- topDirectory
        let path      = buildPath context
            pkgConfig = path -/- "inplace-pkg-config"
            oldPath   = top -/- path </> "build"

        fixFile pkgConfig $ unlines . map (replace oldPath path) . lines

        buildWithResources rs $ Target context (GhcPkg stage) [pkgConfig] [conf]

    when (package == rts && stage == Stage1) $ do
        packageDbDirectory Stage1 -/- "rts.conf" %> \conf -> do
            need [rtsConf]
            buildWithResources rs $ Target context (GhcPkg stage) [rtsConf] [conf]

        rtsConf %> \_ -> do
            need [pkgDataFile rtsContext, rtsConfIn]
            build $ Target context HsCpp [rtsConfIn] [rtsConf]

            let fixRtsConf = unlines
                           . map
                           ( replace "\"\"" ""
                           . replace "rts/dist/build" rtsBuildPath
                           . replace "includes/dist-derivedconstants/header" generatedPath )
                           . filter (not . null)
                           . lines

            fixFile rtsConf fixRtsConf

    when (package == ghc) $ packageDbStamp stage %> \stamp -> do
        removeDirectory dir
        buildWithResources rs $ Target (vanillaContext stage ghc) (GhcPkg stage) [] [dir]
        writeFileLines stamp []
        putSuccess $ "| Successfully initialised " ++ dir
