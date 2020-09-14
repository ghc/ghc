module PurePlugin where

import GHC.Plugins
import Common

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = Just install,
    pluginRecompile  = purePlugin
  }
