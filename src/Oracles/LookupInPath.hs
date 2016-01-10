{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Oracles.LookupInPath (lookupInPath, lookupInPathOracle) where

import Base

newtype LookupInPath = LookupInPath String
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- | Fetches the absolute FilePath to a given FilePath using the oracle.
commandPath :: FilePath -> Action FilePath
commandPath = askOracle . LookupInPath

-- | Lookup a @command@ in @PATH@ environment.
lookupInPath :: FilePath -> Action FilePath
lookupInPath c
    | c /= takeFileName c = return c
    | otherwise           = commandPath c

lookupInPathOracle :: Rules ()
lookupInPathOracle = do
    answer <- newCache $ \query -> do
        envPaths <- wordsBy (== ':') <$> getEnvWithDefault "" "PATH"
        let candidates = map (-/- query) envPaths
        -- this will crash if we do not find any valid candidate.
        fullCommand <- head <$> filterM doesFileExist candidates
        putOracle $ "Found '" ++ query ++ "' at " ++ "'" ++ fullCommand ++ "'"
        return fullCommand
    _ <- addOracle $ \(LookupInPath query) -> answer query
    return ()
