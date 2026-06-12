module MyPlugin (plugin) where

import GHC.Plugins

-- A no-op plugin: it makes no changes to the program but, to run at all, it
-- must be found and loaded by the consumer module's compilation.
plugin :: Plugin
plugin = defaultPlugin { pluginRecompile = purePlugin }
