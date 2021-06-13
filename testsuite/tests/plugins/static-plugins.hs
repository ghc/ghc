module Main where

import GHC.Driver.Env
import GHC.Driver.Session
  (extractDynFlags, parseDynamicFlagsCmdLine, defaultFatalMessager, defaultFlushOut)
import GHC.Driver.Plugins
import GHC.Driver.Monad

import GHC.Types.Avail
import Control.Monad.IO.Class
import GHC
import GHC.Fingerprint.Type
import GHC.Hs.Decls
import GHC.Hs.Doc
import GHC.Hs.Expr
import Language.Haskell.Syntax.Extension
import GHC.Hs.ImpExp
import GHC.Utils.Outputable
import System.Environment
import GHC.Tc.Types
import GHC.Run

import Simple.SourcePlugin (plugin)

main = do
  libdir:args <- getArgs
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhcWithAbiHashes (Just libdir) $ do
      dflags <- getSessionDynFlags
      -- liftIO $ print args
      -- (dflags,_,_)
      --     <- parseDynamicFlagsCmdLine dflags (map noLoc args)
      -- we need to LinkInMemory otherwise `setTarget [] >> load LoadAllTargets`
      -- below will fail.
      setSessionDynFlags dflags { ghcLink = LinkInMemory}

      -- Start with a pure plugin, this should trigger recomp.
      liftIO $ putStrLn "==pure.0"
      loadWithPlugins [StaticPlugin $ PluginWithArgs plugin0_pure []]

      -- The same (or a different) pure plugin shouldn't trigger recomp.
      liftIO $ putStrLn "==pure.1"
      loadWithPlugins [StaticPlugin $ PluginWithArgs plugin0_pure []]

      -- Next try with a fingerprint plugin, should trigger recomp.
      liftIO $ putStrLn "==fp0.0"
      loadWithPlugins [StaticPlugin $ PluginWithArgs plugin_fp0 []]

      -- With the same fingerprint plugin, should not trigger recomp.
      liftIO $ putStrLn "==fp0.1"
      loadWithPlugins [StaticPlugin $ PluginWithArgs plugin_fp0 []]

      -- Change the plugin fingerprint, should trigger recomp.
      liftIO $ putStrLn "==fp1"
      loadWithPlugins [StaticPlugin $ PluginWithArgs plugin_fp1 []]

      -- TODO: this currently doesn't work, patch pending
      -- -- Even though the plugin is now pure we should still recomp since we
      -- -- used a potentially impure plugin before
      -- liftIO $ putStrLn "pure.2"
      -- loadWithPlugins [StaticPlugin $ PluginWithArgs plugin0_pure []]

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


plugin_fp0   =
  plugin { pluginRecompile = \_ -> pure $ MaybeRecompile $ Fingerprint 0 0 }
plugin_fp1   =
  plugin { pluginRecompile = \_ -> pure $ MaybeRecompile $ Fingerprint 0 1 }
plugin0_pure =
  plugin { pluginRecompile = \_ -> pure $ NoForceRecompile }
