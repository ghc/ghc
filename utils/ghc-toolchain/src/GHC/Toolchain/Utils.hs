{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Toolchain.Utils
    ( expectJust
    , expectFileExists
    , withTempDir
    , oneOf
    ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import System.IO.Error

import GHC.Toolchain.Prelude

createTempDirectory
    :: forall m. (MonadIO m, MonadCatch m)
    => m FilePath
createTempDirectory = do
    root <- liftIO $ getTemporaryDirectory
    go root 0
  where
    go :: FilePath -> Int -> m FilePath
    go root n = do
        let path = root </> "tmp"++show n
        res <- try $ liftIO $ createDirectory path
        case res of
          Right () -> return path
          Left err
            | isAlreadyExistsError err -> go root (n+1)
            | otherwise -> throwM err

withTempDir :: (FilePath -> M a) -> M a
withTempDir f = do
    env <- getEnv
    let close dir
          | keepTemp env = return ()
          | otherwise    = liftIO $ removeDirectoryRecursive dir
    bracket createTempDirectory close f

expectJust :: String -> Maybe a -> M a
expectJust err Nothing = throwE err
expectJust _   (Just x) = return x

expectFileExists :: FilePath -> String -> M ()
expectFileExists path err = do
    exists <- liftIO $ doesFileExist path
    unless exists $ throwE err

oneOf :: String -> [M b] -> M b
oneOf err = foldr (<|>) (throwE err)
