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
lookupInPathOracle = void $
    addOracle $ \(LookupInPath name) -> do
        maybePath <- liftIO $ findExecutable name
        path <- case maybePath of
            Just value -> return $ unifyPath value
            Nothing    -> putError $ "Cannot find executable '" ++ name ++ "'."
        putOracle $ "Executable found: " ++ name ++ " => " ++ path
        return path
