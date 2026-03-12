module GHC.Core.Opt.CompUnit
  ( parMapCompUnits
  , coreCompUnitTimingDoc
  , forceCompUnit
  ) where

import GHC.Prelude

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, evaluate, throwIO, try)
import System.IO.Unsafe (unsafePerformIO)

import GHC.Driver.Flags (DumpFlag(Opt_D_dump_timings))
import GHC.Core
import GHC.Core.Stats (coreBindsSize)
import GHC.Core.Seq (seqBinds, seqRules)
import GHC.Utils.Error (withTiming)
import GHC.Utils.Logger (Logger, logHasDumpFlag)
import GHC.Utils.Outputable
import Debug.Trace (traceEventIO)

parMapCompUnits :: Logger -> String -> (CoreCompUnit -> CoreCompUnit) -> CoreProgram -> CoreProgram
parMapCompUnits logger pass_name f units = unsafePerformIO $ do
    result_vars <- mapM (uncurry fork_unit) (zip [1 :: Int ..] units)
    mapM take_unit result_vars
  where
    total_units = length units
    do_timings = total_units > 1 && logHasDumpFlag logger Opt_D_dump_timings

    fork_unit unit_no unit = do
      result_var <- newEmptyMVar
      _ <- forkIO $ do
        traceEventIO ("parMapCompUnits: Start(" ++ (show unit_no) ++ "): " ++ pass_name)
        result <- try $
          if do_timings
            then withTiming logger (coreCompUnitTimingDoc pass_name unit_no total_units unit) forceCompUnit $
                   evaluate $ let unit' = f unit in forceCompUnit unit' `seq` unit'
            else evaluate $ let unit' = f unit in forceCompUnit unit' `seq` unit'
        putMVar result_var result
        traceEventIO ("parMapCompUnits: End(" ++ (show unit_no) ++ "): " ++ pass_name)
      pure result_var

    take_unit result_var = do
      result <- takeMVar result_var
      case result of
        Left err -> throwIO (err :: SomeException)
        Right unit -> pure unit

forceCompUnit :: CoreCompUnit -> ()
forceCompUnit (CoreCompUnit unit_binds unit_rules) =
  seqBinds unit_binds `seq` seqRules unit_rules `seq` ()

coreCompUnitTimingDoc :: String -> Int -> Int -> CoreCompUnit -> SDoc
coreCompUnitTimingDoc pass_name unit_no total_units (CoreCompUnit unit_binds unit_rules) =
  text pass_name
    <+> parens
          (text "unit"
           <+> int unit_no <> char '/' <> int total_units <> comma
           <+> text "binds=" <> int (length unit_binds) <> comma
           <+> text "rules=" <> int (length unit_rules) <> comma
           <+> text "size=" <> int (coreBindsSize unit_binds))
