module Main where

import Control.Monad.IO.Class
import GHC
import GHC.Driver.Monad
import GHC.Plugins
import System.Environment

main = do
  libdir:args <- getArgs
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      -- we need to LinkInMemory otherwise `setTarget [] >> load LoadAllTargets`
      -- below will fail.
      setSessionDynFlags dflags { ghcLink = LinkInMemory}

      liftIO $ putStrLn "loading with driver plugin"
      loadWithPlugins [StaticPlugin (PluginWithArgs plugin []) False]


  where
    loadWithPlugins the_plugins = do
      -- first unload (like GHCi :load does)
      GHC.setTargets []
      _ <- GHC.load LoadAllTargets

      target <- guessTarget "static-plugins-module.hs" Nothing Nothing
      setTargets [target]

      modifySession $ \hsc_env ->
        let old_plugins = hsc_plugins hsc_env
        in hsc_env { hsc_plugins = old_plugins { staticPlugins = the_plugins } }

      dflags <- getSessionDynFlags
      setSessionDynFlags dflags { outputFile_ = Nothing }
      load LoadAllTargets
      liftIO (putStrLn "loading done")


plugin   =
  defaultPlugin { driverPlugin = \_ env -> liftIO (putStrLn "driver plugin ran") >> pure env }
