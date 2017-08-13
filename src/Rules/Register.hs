module Rules.Register (registerPackage) where

import Base
import Context
import GHC
import Settings.Path
import Target
import UserSettings
import Util

-- | Build rules for registering packages and initialising package databases
-- by running the @ghc-pkg@ utility.
registerPackage :: [(Resource, Int)] -> Context -> Rules ()
registerPackage rs context@Context {..} = when (stage <= Stage1) $ do
    let confIn = pkgInplaceConfig context
        dir    = inplacePackageDbDirectory stage

    matchVersionedFilePath (dir -/- pkgNameString package) "conf" ?> \conf -> do
        need [confIn]
        buildWithResources rs $
            target context (GhcPkg Update stage) [confIn] [conf]

    when (package == ghc) $ packageDbStamp stage %> \stamp -> do
        removeDirectory dir
        buildWithResources rs $
            target (vanillaContext stage ghc) (GhcPkg Init stage) [] [dir]
        writeFileLines stamp []
        putSuccess $ "| Successfully initialised " ++ dir
