{-# LANGUAGE TemplateHaskell #-}

module Simple.Plugin(plugin) where

import GHC.Types.Unique.FM
import GHC.Plugins
import qualified GHC.Utils.Error

import Control.Monad
import Data.Monoid hiding (Alt)
import Data.Dynamic
import qualified Language.Haskell.TH as TH

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install,
    pluginRecompile  = purePlugin
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install options todos = do
    putMsgS $ "Simple Plugin Passes Queried"
    putMsgS $ "Got options: " ++ unwords options

    -- Create some actual passes to continue the test.
    return todos
