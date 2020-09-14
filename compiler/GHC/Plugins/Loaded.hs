module GHC.Plugins.Loaded
   ( LoadedPlugin(..), lpModuleName,
   )
where

import GHC.Prelude

import GHC.Plugins.Types

import GHC.Driver.Types
import GHC.Unit.Types
import GHC.Unit.Module.Name

-- | A plugin with its arguments. The result of loading the plugin.
data LoadedPlugin = LoadedPlugin
  { lpPlugin :: PluginWithArgs
  -- ^ the actual plugin together with its commandline arguments
  , lpModule :: ModIface
  -- ^ the module containing the plugin
  }

-- | Return the ModuleName of a loaded plugin
lpModuleName :: LoadedPlugin -> ModuleName
lpModuleName = moduleName . mi_module . lpModule

