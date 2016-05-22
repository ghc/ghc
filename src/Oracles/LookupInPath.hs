{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Oracles.LookupInPath (lookupInPath, lookupInPathOracle) where

import System.Directory

import Base

newtype LookupInPath = LookupInPath String
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

-- | Lookup an executable in @PATH@.
lookupInPath :: FilePath -> Action FilePath
lookupInPath name
    | name == takeFileName name = askOracle $ LookupInPath name
    | otherwise                 = return name

lookupInPathOracle :: Rules ()
lookupInPathOracle = void $
    addOracle $ \(LookupInPath name) -> do
        let unpack = fromMaybe . error $ "Cannot find executable " ++ quote name
        path <- unifyPath <$> unpack <$> liftIO (findExecutable name)
        putLoud $ "Executable found: " ++ name ++ " => " ++ path
        return path
