-- The plugins datatype is stored in DynFlags, so it needs to be
-- exposed without importing all of its implementation.
module GHC.Driver.Plugins where

import GHC.Prelude ()

data Plugin
data Plugins

emptyPlugins :: Plugins

data LoadedPlugin
data StaticPlugin
