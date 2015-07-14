{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.ArgsHash (
    ArgsHashKey (..), askArgsHash, argsHashOracle
    ) where

import Development.Shake.Classes
import Base
import Settings
import Expression

newtype ArgsHashKey = ArgsHashKey FullTarget
                      deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

askArgsHash :: FullTarget -> Action Int
askArgsHash = askOracle . ArgsHashKey

-- Oracle for storing per-target argument list hashes
argsHashOracle :: Rules ()
argsHashOracle = do
    addOracle $ \(ArgsHashKey target) -> hash <$> interpret target args
    return ()
