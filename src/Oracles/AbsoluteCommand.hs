{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Oracles.AbsoluteCommand (
    lookupInPathOracle, absoluteCommandOracle
    ) where

import Base

newtype AbsoluteCommand = AbsoluteCommand String
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- | Fetches the absolute FilePath to a given FilePath from the
-- Oracle.
absoluteCommand :: FilePath -> Action FilePath
absoluteCommand = askOracle . AbsoluteCommand

-- | Lookup a @command@ in @PATH@ environment.
lookupInPathOracle :: FilePath -> Action FilePath
lookupInPathOracle c
    | c /= takeFileName c = return c
    | otherwise           = absoluteCommand c

absoluteCommandOracle :: Rules ()
absoluteCommandOracle = do
    o <- newCache $ \c -> do
        envPaths <- wordsWhen (== ':') <$> getEnvWithDefault "" "PATH"
        let candidates = map (-/- c) envPaths
        -- this will crash if we do not find any valid candidate.
        fullCommand <- head <$> filterM doesFileExist candidates
        putOracle $ "Found '" ++ c ++ "' at " ++ "'" ++ fullCommand ++ "'"
        return fullCommand
    _ <- addOracle $ \(AbsoluteCommand c) -> o c
    return ()
