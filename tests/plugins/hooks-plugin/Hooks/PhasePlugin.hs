{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}
module Hooks.PhasePlugin (plugin) where

import GHC.Plugins
import GHC.Driver.Hooks
import GHC.Tc.Utils.Monad
import GHC.Driver.Pipeline.Execute
import GHC.Driver.Pipeline.Phases
import System.IO

plugin :: Plugin
plugin = defaultPlugin { driverPlugin = hooksP }

hooksP :: [CommandLineOption] -> HscEnv -> IO HscEnv
hooksP opts hsc_env = do
  hSetBuffering stdout NoBuffering
  let hooks  = hsc_hooks hsc_env
      hooks' = hooks { runPhaseHook = Just fakeRunPhaseHook }
      hsc_env' = hsc_env { hsc_hooks = hooks' }
  return hsc_env'

fakeRunPhaseHook :: PhaseHook
fakeRunPhaseHook = PhaseHook $ \tPhase -> do
  liftIO $ case tPhase of
    T_Cpp{} -> putStrLn "Cpp hook fired"
    T_Hsc{} -> putStrLn "Hsc hook fired"
    T_FileArgs{} -> putStrLn "FileArgs hook fired"
    _ -> pure ()
  runPhase tPhase
