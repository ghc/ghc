{-# LANGUAGE CPP #-}
module Distribution.Compat.Exception (
  catchIO,
  catchExit,
  tryIO,
  displayException,
  ) where

import System.Exit
import qualified Control.Exception as Exception
#if __GLASGOW_HASKELL__ >= 710
import Control.Exception (displayException)
#endif

tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO = Exception.try

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

catchExit :: IO a -> (ExitCode -> IO a) -> IO a
catchExit = Exception.catch

#if __GLASGOW_HASKELL__ < 710
displayException :: Exception.Exception e => e -> String
displayException = show
#endif
