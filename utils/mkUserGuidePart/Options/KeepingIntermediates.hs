module Options.KeepingIntermediates where

import Types

keepingIntermediatesOptions :: [Flag]
keepingIntermediatesOptions =
  [ flag { flagName = "-keep-hc-file, -keep-hc-files"
         , flagDescription = "retain intermediate ``.hc`` files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-keep-llvm-file, -keep-llvm-files"
         , flagDescription = "retain intermediate LLVM ``.ll`` files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-keep-s-file, -keep-s-files"
         , flagDescription = "retain intermediate ``.s`` files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-keep-tmp-files"
         , flagDescription = "retain all intermediate temporary files"
         , flagType = DynamicFlag
         }
  ]
