{-# OPTIONS_GHC -fplugin Simple.RemovePlugin #-}
{-# OPTIONS_GHC -fplugin-opt Simple.RemovePlugin:map #-}
{-# OPTIONS_GHC -fplugin-opt Simple.RemovePlugin:parse #-}
-- testing that the plugin can alter the parsed representation
module A where

map x = ()

x = map show [1,2,3]
