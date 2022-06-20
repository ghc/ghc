module Hooks.LogPlugin (plugin) where

import GHC.Plugins
import GHC.Driver.Hooks
import GHC.Tc.Utils.Monad
import GHC.Utils.Logger
import GHC.Driver.Pipeline.Execute
import System.IO

plugin :: Plugin
plugin = defaultPlugin { driverPlugin = hooksP }

hooksP :: [CommandLineOption] -> HscEnv -> IO HscEnv
hooksP opts hsc_env = do
  hSetBuffering stdout NoBuffering
  let logger  = hsc_logger hsc_env
      logger' = pushLogHook logHook logger
      hsc_env' = hsc_env { hsc_logger = logger' }
  return hsc_env'

logHook :: LogAction -> LogAction
logHook action logFlags messageClass srcSpan msgDoc = do
  putStrLn "Log hook called"
  action logFlags messageClass srcSpan msgDoc
