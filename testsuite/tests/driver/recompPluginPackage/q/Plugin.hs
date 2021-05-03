module Plugin where

import GHC.Plugins
import Lib

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  , pluginRecompile = purePlugin
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  liftIO (print p)
  return todo
