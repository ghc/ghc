module LinkerTicklingPlugin where

import GHC.Plugins
import GHC.Driver.Session

plugin :: Plugin
plugin = defaultPlugin {
        installCoreToDos = install
    }

-- This tests whether plugins are linking against the *running* GHC or a new
-- instance of it. If it is a new instance (settings unsafeGlobalDynFlags) won't
-- have been initialised, so we'll get a GHC panic here:
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _options todos = settings unsafeGlobalDynFlags `seq` return todos
