module Options.Phases where

import Types

phaseOptions :: [Flag]
phaseOptions =
  [ flag { flagName = "-F"
         , flagDescription =
           "Enable the use of a :ref:`pre-processor <pre-processor>` "++
           "(set with :ghc-flag:`-pgmF`)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-E"
         , flagDescription = "Stop after preprocessing (``.hspp`` file)"
         , flagType = ModeFlag
         }
  , flag { flagName = "-C"
         , flagDescription = "Stop after generating C (``.hc`` file)"
         , flagType = ModeFlag
         }
  , flag { flagName = "-S"
         , flagDescription = "Stop after generating assembly (``.s`` file)"
         , flagType = ModeFlag
         }
  , flag { flagName = "-c"
         , flagDescription = "Stop after generating object (``.o``) file"
         , flagType = ModeFlag
         }
  , flag { flagName = "-x⟨suffix⟩"
         , flagDescription = "Override default behaviour for source files"
         , flagType = DynamicFlag
         }
  ]
