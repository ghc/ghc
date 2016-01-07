{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Oracles.LookupInPath (
    lookupInPath, lookupInPathOracle
    ) where

import Base

newtype LookupInPath = LookupInPath String
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- | Fetches the absolute FilePath to a given FilePath from the
-- Oracle.
commandPath :: FilePath -> Action FilePath
commandPath = askOracle . LookupInPath

-- | Lookup a @command@ in @PATH@ environment.
lookupInPath :: FilePath -> Action FilePath
lookupInPath c
    | c /= takeFileName c = return c
    | otherwise           = commandPath c

lookupInPathOracle :: Rules ()
lookupInPathOracle = do
    o <- newCache $ \c -> do
        envPaths <- wordsWhen (== ':') <$> getEnvWithDefault "" "PATH"
        let candidates = map (-/- c) envPaths
        -- this will crash if we do not find any valid candidate.
        fullCommand <- head <$> filterM doesFileExist candidates
        putOracle $ "Found '" ++ c ++ "' at " ++ "'" ++ fullCommand ++ "'"
        return fullCommand
    _ <- addOracle $ \(LookupInPath c) -> o c
    return ()
