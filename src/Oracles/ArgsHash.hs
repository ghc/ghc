{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.ArgsHash (
    checkArgsHash, argsHashOracle
    ) where

import Base
import Target
import Expression
import Settings.Args
import Control.Applicative

newtype ArgsHashKey = ArgsHashKey Target
    deriving (Show, Eq, Typeable, Binary, Hashable, NFData)

-- This is an action that given a full target determines the corresponding
-- argument list and computes its hash. The resulting value is tracked in a
-- Shake oracle, hence initiating rebuilts when the hash is changed (a hash
-- change indicates changes in the build system).
-- Note: we replace target sources with ["src"] for performance reasons -- to
-- avoid storing long lists of source files passed to some builders (e.g. Ar)
-- in the Shake database. This optimisation is harmless, because argument list
-- constructors are assumed not to examine target sources, but only append them
-- to argument lists where appropriate.
-- TODO: enforce the above assumption via type trickery?
checkArgsHash :: FullTarget -> Action ()
checkArgsHash target = do
    tmp <- askOracle . ArgsHashKey $ target { sources = ["src"] } :: Action Int
    return ()

-- Oracle for storing per-target argument list hashes
argsHashOracle :: Rules ()
argsHashOracle = do
    addOracle $ \(ArgsHashKey target) -> hash <$> interpret target getArgs
    return ()
