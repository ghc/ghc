module ImpurePlugin where

import GHC.Plugins
import Common

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install,
    pluginRecompile = impurePlugin
  }
