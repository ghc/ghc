{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Oracles.ArgsHash (checkArgsHash, argsHashOracle) where

import Base
import Builder
import Expression
import Settings
import Target

newtype ArgsHashKey = ArgsHashKey Target
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

-- TODO: Hash Target to improve accuracy and performance.
-- | Given a full target this Action determines the corresponding argument list
-- and computes its hash. The resulting value is tracked in a Shake oracle,
-- hence initiating rebuilds when the hash changes (a hash change indicates
-- changes in the build command for the given target).
-- Note: we keep only the first target input for performance reasons -- to
-- avoid storing long lists of source files passed to some builders (e.g. Ar)
-- in the Shake database. This optimisation is normally harmless, because
-- argument list constructors are assumed not to examine target sources, but
-- only append them to argument lists where appropriate.
checkArgsHash :: Target -> Action ()
checkArgsHash t = do
    let hashedInputs = [ show $ hash (inputs t) ]
        hashedTarget = target (context t) (builder t) hashedInputs (outputs t)
    void (askOracle $ ArgsHashKey hashedTarget :: Action Int)

-- | Oracle for storing per-target argument list hashes.
argsHashOracle :: Rules ()
argsHashOracle = void $
    addOracle $ \(ArgsHashKey target) -> do
        argList <- interpret target getArgs
        let trackedArgList = filter (trackedArgument $ builder target) argList
        return $ hash trackedArgList
