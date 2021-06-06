module Simple.DefaultPlugin (plugin) where
import GHC.Driver.Plugins (Plugin, defaultPlugin)

plugin :: Plugin
plugin = defaultPlugin
