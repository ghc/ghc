{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ConstraintKinds #-}

module Oracles (
    module Oracles.Base,
    module Oracles.Flag,
    module Oracles.Option,
    module Oracles.Builder,
    module Oracles.PackageData,
    oracleRules
    ) where

import Development.Shake.Config
import qualified Data.HashMap.Strict as M
import Base
import Config
import Oracles.Base
import Oracles.Flag
import Oracles.Option
import Oracles.Builder
import Oracles.PackageData

oracleRules :: Rules ()
oracleRules = do
    cfg <- newCache $ \() -> do
        unless (doesFileExist $ cfgPath </> "default.config.in") $ do
            error $ "\nDefault configuration file '"
                ++ (cfgPath </> "default.config.in")
                ++ "' is missing; unwilling to proceed."
            return ()
        need [cfgPath </> "default.config"]
        cfgDefault <- liftIO $ readConfigFile $ cfgPath </> "default.config"
        existsUser <- doesFileExist $ cfgPath </> "user.config"
        cfgUser    <- if existsUser
                      then liftIO $ readConfigFile $ cfgPath </> "user.config"
                      else do
                          putLoud $ "\nUser defined configuration file '"
                              ++ (cfgPath </> "user.config")
                              ++ "' is missing; proceeding with default configuration.\n"
                          return M.empty
        return $ cfgUser `M.union` cfgDefault

    addOracle $ \(ConfigKey key) -> M.lookup key <$> cfg ()

    pkgData <- newCache $ \file -> do
        need [file]
        liftIO $ readConfigFile file

    addOracle $ \(PackageDataPair (file, key)) -> M.lookup key <$> pkgData file
    return ()
