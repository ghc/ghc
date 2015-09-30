module Options.PlatformSpecific where

import Types

platformSpecificOptions :: [Flag]
platformSpecificOptions =
  [ flag { flagName = "-msse2"
         , flagDescription = "(x86 only) Use SSE2 for floating-point operations"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-msse4.2"
         , flagDescription = "(x86 only) Use SSE4.2 for floating-point operations"
         , flagType = DynamicFlag
         }
  ]
