{-# LANGUAGE RankNTypes #-}
module Plugins (
    FrontendPlugin(..), defaultFrontendPlugin, FrontendPluginAction,
    Plugin(..), CommandLineOption, LoadedPlugin(..),
    defaultPlugin, withPlugins, withPlugins_
    ) where

import GhcPrelude

import CoreMonad ( CoreToDo, CoreM )
import TcRnTypes ( TcPlugin)
import DynFlags
import GhcMonad
import DriverPhases
import Module ( ModuleName )

import Control.Monad

-- | Command line options gathered from the -PModule.Name:stuff syntax
-- are given to you as this type
type CommandLineOption = String

-- | 'Plugin' is the compiler plugin data type. Try to avoid
-- constructing one of these directly, and just modify some fields of
-- 'defaultPlugin' instead: this is to try and preserve source-code
-- compatibility when we add fields to this.
--
-- Nonetheless, this API is preliminary and highly likely to change in
-- the future.
data Plugin = Plugin {
    installCoreToDos :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
    -- ^ Modify the Core pipeline that will be used for compilation.
    -- This is called as the Core pipeline is built for every module
    -- being compiled, and plugins get the opportunity to modify the
    -- pipeline in a nondeterministic order.
  , tcPlugin :: [CommandLineOption] -> Maybe TcPlugin
    -- ^ An optional typechecker plugin, which may modify the
    -- behaviour of the constraint solver.
  }

-- | A plugin with its arguments. The result of loading the plugin.
data LoadedPlugin = LoadedPlugin {
    lpPlugin :: Plugin
    -- ^ the actual callable plugin
  , lpModuleName :: ModuleName
    -- ^ the qualified name of the module containing the plugin
  , lpArguments :: [CommandLineOption]
    -- ^ command line arguments for the plugin
  }

-- | Default plugin: does nothing at all! For compatibility reasons
-- you should base all your plugin definitions on this default value.
defaultPlugin :: Plugin
defaultPlugin = Plugin {
        installCoreToDos = const return
      , tcPlugin         = const Nothing
    }

type PluginOperation m a = Plugin -> [CommandLineOption] -> a -> m a
type ConstPluginOperation m a = Plugin -> [CommandLineOption] -> a -> m ()

-- | Perform an operation by using all of the plugins in turn.
withPlugins :: Monad m => DynFlags -> PluginOperation m a -> a -> m a
withPlugins df transformation input
  = foldM (\arg (LoadedPlugin p _ opts) -> transformation p opts arg)
          input (plugins df)

-- | Perform a constant operation by using all of the plugins in turn.
withPlugins_ :: Monad m => DynFlags -> ConstPluginOperation m a -> a -> m ()
withPlugins_ df transformation input
  = mapM_ (\(LoadedPlugin p _ opts) -> transformation p opts input)
          (plugins df)

type FrontendPluginAction = [String] -> [(String, Maybe Phase)] -> Ghc ()
data FrontendPlugin = FrontendPlugin {
      frontend :: FrontendPluginAction
    }
defaultFrontendPlugin :: FrontendPlugin
defaultFrontendPlugin = FrontendPlugin { frontend = \_ _ -> return () }
