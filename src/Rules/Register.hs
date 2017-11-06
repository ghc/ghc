module Rules.Register (registerPackage) where

import Base
import Context
import GHC
import Target
import Utilities

-- TODO: Simplify.
-- | Build rules for registering packages and initialising package databases
-- by running the @ghc-pkg@ utility.
registerPackage :: [(Resource, Int)] -> Context -> Rules ()
registerPackage rs context@Context {..} = do
    when (stage == Stage0) $ do
        -- Packages @ghc-boot@ and @ghc-boot-th@ both match the @ghc-boot*@
        -- pattern, therefore we need to use priorities to match the right rule.
        -- TODO: Get rid of this hack.
        "//" ++ stage0PackageDbDir -/- pkgName package ++ "*.conf" %%>
            buildConf rs context

        when (package == ghc) $ "//" ++ stage0PackageDbDir -/- packageDbStamp %>
            buildStamp rs context

    when (stage == Stage1) $ do
        inplacePackageDbPath -/- pkgName package ++ "*.conf" %%>
            buildConf rs context

        when (package == ghc) $ inplacePackageDbPath -/- packageDbStamp %>
            buildStamp rs context

buildConf :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildConf rs context@Context {..} conf = do
    confIn <- pkgInplaceConfig context
    need [confIn]
    buildWithResources rs $ target context (GhcPkg Update stage) [confIn] [conf]

buildStamp :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildStamp rs Context {..} stamp = do
    let path = takeDirectory stamp
    removeDirectory path
    buildWithResources rs $
        target (vanillaContext stage ghc) (GhcPkg Init stage) [] [path]
    writeFileLines stamp []
    putSuccess $ "| Successfully initialised " ++ path
