-- Tests panic in handleTHMessage is handled properly.
-- Plugin is just a convenient way to inject a panic.
module Main where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

import GHC
import GHC.Driver.Env (hsc_plugins)
import GHC.Driver.Monad (modifySession)
import GHC.Driver.Plugins
import GHC.Driver.Session (defaultFatalMessager, defaultFlushOut, gopt_set)
import GHC.Tc.Types (IfM)
import GHC.Unit.Module.ModIface (ModIface, mi_module)
import GHC.Utils.Panic (panic)

main :: IO ()
main = do
  libdir:_ <- getArgs
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags (gopt_set dflags Opt_ExternalInterpreter)
                           { ghcLink = LinkInMemory }

      modifySession $ \hsc_env ->
        hsc_env { hsc_plugins = (hsc_plugins hsc_env)
                    { staticPlugins = [StaticPlugin (PluginWithArgs panicPlugin []) False] } }

      target <- guessTarget "static-plugin-interp-panic-module.hs" Nothing Nothing
      setTargets [target]
      ok <- load LoadAllTargets
      liftIO $ putStrLn ("load succeeded: " ++ show (succeeded ok))

panicPlugin :: Plugin
panicPlugin = defaultPlugin
  { interfaceLoadAction = ifaceAction
  , pluginRecompile     = purePlugin
  }

ifaceAction :: [CommandLineOption] -> ModIface -> IfM lcl ModIface
ifaceAction _ iface
  | modName == "GHC.Internal.Real" = panic ("static-plugin-interp-panic: panic loading " ++ modName)
  | otherwise                      = return iface
  where
    modName = moduleNameString (moduleName (mi_module iface))
