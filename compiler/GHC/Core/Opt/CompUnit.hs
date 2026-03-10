module GHC.Core.Opt.CompUnit
  ( parMapCompUnits
  ) where

import GHC.Prelude

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, evaluate, throwIO, try)
import System.IO.Unsafe (unsafePerformIO)

import GHC.Core
import GHC.Core.Seq (seqBinds, seqRules)

parMapCompUnits :: (CoreCompUnit -> CoreCompUnit) -> CoreProgram -> CoreProgram
parMapCompUnits f units = unsafePerformIO $ do
    result_vars <- mapM fork_unit units
    mapM take_unit result_vars
  where
    fork_unit unit = do
      result_var <- newEmptyMVar
      _ <- forkIO $ do
        result <- try $ evaluate $ forceCompUnit (f unit)
        putMVar result_var result
      pure result_var

    take_unit result_var = do
      result <- takeMVar result_var
      case result of
        Left err -> throwIO (err :: SomeException)
        Right unit -> pure unit

    forceCompUnit unit@(CoreCompUnit unit_binds unit_rules) =
      seqBinds unit_binds `seq` seqRules unit_rules `seq` unit
