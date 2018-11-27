module TcPluginGHCi where

import TcRnMonad ( TcPlugin(..), TcPluginResult(..) )
import Plugins ( defaultPlugin, Plugin(..), CommandLineOption )
import Debug.Trace

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . thePlugin }

thePlugin :: [CommandLineOption] -> TcPlugin
thePlugin opts = TcPlugin
  { tcPluginInit  = trace "TcPluginGHCi" (return ())
  , tcPluginSolve = \_ _ _ _ -> return $ TcPluginOk [] []
  , tcPluginStop  = \_ -> return ()
  }
