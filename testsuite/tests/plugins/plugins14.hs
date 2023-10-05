{-# OPTIONS_GHC -fplugin Simple.RemovePlugin #-}
{-# OPTIONS_GHC -fplugin-opt Simple.RemovePlugin:map #-}
{-# OPTIONS_GHC -fplugin-opt Simple.RemovePlugin:interface #-}
module A where
-- test if a definition can be removed from loaded interface

map :: ()
map = ()

x :: ()
x = map
