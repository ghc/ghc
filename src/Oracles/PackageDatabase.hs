module Oracles.PackageDatabase (packageDatabaseOracle) where

import qualified System.Directory as IO

import Base
import Context
import Builder
import GHC
import Rules.Actions
import Settings.Builders.GhcCabal
import Settings.Paths
import Target
import UserSettings

packageDatabaseOracle :: Rules ()
packageDatabaseOracle = void $
    addOracle $ \(PackageDatabaseKey stage) -> do
        let dir  = packageDbDirectory stage
            file = dir -/- "package.cache"
        unlessM (liftIO $ IO.doesFileExist file) $ do
            removeDirectory dir
            build $ Target (vanillaContext stage ghcPkg) (GhcPkg stage) [] [dir]
            putSuccess $ "| Successfully initialised " ++ dir
