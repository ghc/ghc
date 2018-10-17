module PurePlugin where

import GhcPlugins
import Common

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install,
    pluginRecompile  = purePlugin
  }
