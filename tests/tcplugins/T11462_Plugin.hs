module T11462_Plugin(plugin) where

import GHC.Tc.Utils.Monad ( TcPlugin(..), TcPluginSolveResult(..), TcPluginRewriteResult(..) )
import GHC.Driver.Plugins ( defaultPlugin, Plugin(..), CommandLineOption )
import GHC.Types.Unique.FM ( emptyUFM )

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . thePlugin }

thePlugin :: [CommandLineOption] -> TcPlugin
thePlugin opts = TcPlugin
  { tcPluginInit    = return ()
  , tcPluginSolve   = \_ _ _ _ -> return $ TcPluginOk [] []
  , tcPluginRewrite = \_ -> emptyUFM
  , tcPluginStop    = \_ -> return ()
  }
