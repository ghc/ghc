module Oracles.PackageDb (packageDbOracle) where

import qualified System.Directory as IO

import Base
import Builder
import GHC
import Rules.Actions
import Settings.Builders.GhcCabal
import Settings.Paths
import Target

packageDbOracle :: Rules ()
packageDbOracle = do
    _ <- addOracle $ \(PackageDbKey stage) -> do
        let dir  = packageDbDirectory stage
            file = dir -/- "package.cache"
        unlessM (liftIO $ IO.doesFileExist file) $ do
            let target  = PartialTarget stage ghcPkg
            removeDirectoryIfExists dir
            build $ fullTarget target (GhcPkg stage) [] [dir]
            putSuccess $ "| Successfully initialised " ++ dir
    return ()
