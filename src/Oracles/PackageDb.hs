module Oracles.PackageDb (packageDbOracle) where

import qualified System.Directory as IO

import Base
import Context
import Builder
import GHC
import Rules.Actions
import Settings.Builders.GhcCabal
import Settings.Paths
import Settings.User
import Target

packageDbOracle :: Rules ()
packageDbOracle = void $
    addOracle $ \(PackageDbKey stage) -> do
        let dir  = packageDbDirectory stage
            file = dir -/- "package.cache"
        unlessM (liftIO $ IO.doesFileExist file) $ do
            removeDirectory dir
            build $ Target (vanillaContext stage ghcPkg) (GhcPkg stage) [] [dir]
            putSuccess $ "| Successfully initialised " ++ dir
