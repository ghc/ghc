module FingerprintPlugin where

import GHC.Plugins
import Common

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install,
    pluginRecompile = flagRecompile
  }
