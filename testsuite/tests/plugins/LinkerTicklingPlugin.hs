module LinkerTicklingPlugin where

import GhcPlugins
import StaticFlags

plugin :: Plugin
plugin = defaultPlugin {
        installCoreToDos = install
    }

-- This tests whether plugins are linking against the *running* GHC
-- or a new instance of it. If it is a new instance the staticFlags
-- won't have been initialised, so we'll get a GHC panic here:
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _options todos = reinitializeGlobals >> (length staticFlags `seq` return todos)
  --- XXX: remove reinitializeGlobals when we have fixed the linker
  -- problem (see comment with reinitializeGlobals in CoreMonad.hs)
