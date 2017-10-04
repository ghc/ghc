module Options.Plugin where

import Types

pluginOptions :: [Flag]
pluginOptions =
  [ flag { flagName = "-fplugin=⟨module⟩"
         , flagDescription = "Load a plugin exported by a given module"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fplugin-opt=⟨module⟩:⟨args⟩"
         , flagDescription =
           "Give arguments to a plugin module; module must be specified with "++
           "``-fplugin``"
         , flagType = DynamicFlag
         }
  ]
