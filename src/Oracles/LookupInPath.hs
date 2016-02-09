{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Oracles.LookupInPath (lookupInPath, lookupInPathOracle) where

import System.Directory

import Base

newtype LookupInPath = LookupInPath String
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- | Lookup an executable in @PATH@.
lookupInPath :: FilePath -> Action FilePath
lookupInPath name
    | name == takeFileName name = askOracle $ LookupInPath name
    | otherwise                 = return name

lookupInPathOracle :: Rules ()
lookupInPathOracle = do
    answer <- newCache $ \query -> do
        maybePath <- liftIO $ findExecutable query
        let path = fromMaybe query maybePath
        putOracle $ "Lookup executable '" ++ query ++ "': " ++ path
        return path
    _ <- addOracle $ \(LookupInPath query) -> answer query
    return ()
