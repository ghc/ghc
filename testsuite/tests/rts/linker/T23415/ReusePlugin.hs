{-# language GHC2021 #-}
module ReusePlugin where

import GHC.Conc.Sync (modifyMVar_)
import GHC.Driver.Env
import GHC.Driver.Plugins (defaultPlugin, Plugin(..), CommandLineOption)
import GHC.Linker.Types
import GHC.Runtime.Interpreter
import GHC.Tc.Plugin
import GHC.Tc.Utils.Monad ( TcPlugin(..), TcPluginSolveResult(..) )
import GHC.Types.Unique.DFM (delFromUDFM)
import GHC.Types.Unique.FM (emptyUFM)
import GHC.Unit (stringToUnitId)

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . thePlugin }

thePlugin :: [CommandLineOption] -> TcPlugin
thePlugin opts = TcPlugin
  { tcPluginInit    = removeLibrary
  , tcPluginSolve   = \_ _ _ _ -> return $ TcPluginOk [] []
  , tcPluginRewrite = \_ -> emptyUFM
  , tcPluginStop    = \_ -> return ()
  }

removeLibrary :: TcPluginM ()
removeLibrary = do
  HscEnv {hsc_interp = Just Interp {interpLoader = Loader loader}} <- getTopEnv
  tcPluginIO (modifyMVar_ loader $ \ ls -> pure (removeOurLib ls))
  where
    removeOurLib = fmap $ \ ls ->
      ls {pkgs_loaded = delFromUDFM (pkgs_loaded ls) (stringToUnitId "reuse-1.0")}
