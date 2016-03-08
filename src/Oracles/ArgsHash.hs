{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Oracles.ArgsHash (checkArgsHash, argsHashOracle) where

import Base
import Expression
import Settings
import Settings.Args
import Target

newtype ArgsHashKey = ArgsHashKey Target
    deriving (Show, Eq, Typeable, Binary, Hashable, NFData)

-- This is an action that given a full target determines the corresponding
-- argument list and computes its hash. The resulting value is tracked in a
-- Shake oracle, hence initiating rebuilts when the hash is changed (a hash
-- change indicates changes in the build system).
-- Note: we keep only the first target input for performance reasons -- to
-- avoid storing long lists of source files passed to some builders (e.g. Ar)
-- in the Shake database. This optimisation is harmless, because argument list
-- constructors are assumed not to examine target sources, but only append them
-- to argument lists where appropriate.
-- TODO: enforce the above assumption via type trickery?
-- TODO: Hash Target to improve accuracy and performance.
checkArgsHash :: Target -> Action ()
checkArgsHash target = when trackBuildSystem $ do
    let hashed = [ show . hash $ inputs target ]
    _ <- askOracle . ArgsHashKey $ target { inputs = hashed } :: Action Int
    return ()

-- Oracle for storing per-target argument list hashes
argsHashOracle :: Rules ()
argsHashOracle = void $
    addOracle $ \(ArgsHashKey target) -> hash <$> interpret target getArgs
