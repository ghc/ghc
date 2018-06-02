{-# OPTIONS_GHC -fplugin Simple.RemovePlugin #-}
{-# OPTIONS_GHC -fplugin-opt Simple.RemovePlugin:clear #-}
{-# OPTIONS_GHC -fplugin-opt Simple.RemovePlugin:meta #-}
{-# LANGUAGE TemplateHaskell #-}
-- testing that the plugin can alter the evaluated splice
module A where

import MetaRemoveHelper

$(clear [d| a = () |])

x = a
