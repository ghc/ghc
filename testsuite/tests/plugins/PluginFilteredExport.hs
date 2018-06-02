{-# OPTIONS_GHC -fplugin Simple.RemovePlugin #-}
{-# OPTIONS_GHC -fplugin-opt Simple.RemovePlugin:map #-}
{-# OPTIONS_GHC -fplugin-opt Simple.RemovePlugin:typecheck #-}
-- testing that the plugin can alter the parsed representation
module PluginFilteredExport where

map :: ()
map = ()
