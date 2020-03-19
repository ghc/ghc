module T11462_Plugin(plugin) where

import GHC.Tc.Utils.Monad ( TcPlugin(..), TcPluginResult(..) )
import GHC.Driver.Plugins ( defaultPlugin, Plugin(..), CommandLineOption )

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . thePlugin }

thePlugin :: [CommandLineOption] -> TcPlugin
thePlugin opts = TcPlugin
  { tcPluginInit  = return ()
  , tcPluginSolve = \_ _ _ _ -> return $ TcPluginOk [] []
  , tcPluginStop  = \_ -> return ()
  }
