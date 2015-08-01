{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.ArgsHash (
    askArgsHash, argsHashOracle
    ) where

import Base
import Expression
import Settings.Args
import Control.Applicative

newtype ArgsHashKey = ArgsHashKey FullTarget
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- This is an action that given a full target determines the corresponding
-- argument list and computes its hash. The resulting value is tracked in a
-- Shake oracle, hence initiating rebuilts when the hash is changed (a hash
-- change indicates changes in the build system).
askArgsHash :: FullTarget -> Action Int
askArgsHash = askOracle . ArgsHashKey

-- Oracle for storing per-target argument list hashes
argsHashOracle :: Rules ()
argsHashOracle = do
    addOracle $ \(ArgsHashKey target) -> hash <$> interpret target args
    return ()
