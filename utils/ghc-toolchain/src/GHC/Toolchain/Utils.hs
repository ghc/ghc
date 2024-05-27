{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Toolchain.Utils
    ( expectJust
    , expectFileExists
    , withTempDir
    , oneOf
    , oneOf'
    , isSuccess
    ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import System.IO.Error
import System.Exit

import GHC.Toolchain.Prelude

createTempDirectory :: IO FilePath
createTempDirectory = do
    root <- getTemporaryDirectory
    go root 0
  where
    go :: FilePath -> Int -> IO FilePath
    go root n = do
        let path = root </> "tmp"++show n
        res <- try $ createDirectory path
        case res of
          Right () -> return path
          Left err
            | isAlreadyExistsError err -> go root (n+1)
            | otherwise -> throwIO err

withTempDir :: (FilePath -> M a) -> M a
withTempDir f = do
    env <- getEnv
    let close dir
          | keepTemp env = return ()
          | otherwise    = removeDirectoryRecursive dir
    makeM (bracket createTempDirectory close (runM env . f))

expectJust :: String -> Maybe a -> M a
expectJust err Nothing = throwE err
expectJust _   (Just x) = return x

expectFileExists :: FilePath -> String -> M ()
expectFileExists path err = do
    exists <- liftIO $ doesFileExist path
    unless exists $ throwE err

oneOf :: String -> [M b] -> M b
oneOf err = oneOf' [err]

-- | Like 'oneOf' but takes a multi-line error message if none of the checks
-- succeed.
oneOf' :: [String] -> [M b] -> M b
oneOf' err = foldr (<|>) (throwEs err)

isSuccess :: ExitCode -> Bool
isSuccess = \case
  ExitSuccess -> True
  ExitFailure _ -> False

