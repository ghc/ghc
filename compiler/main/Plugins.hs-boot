-- The plugins datatype is stored in DynFlags, so it needs to be
-- exposed without importing all of its implementation.
module Plugins where

import GhcPrelude ()

data Plugin

data LoadedPlugin
data StaticPlugin
