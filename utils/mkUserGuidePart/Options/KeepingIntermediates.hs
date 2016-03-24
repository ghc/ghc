module Options.KeepingIntermediates where

import Types

keepingIntermediatesOptions :: [Flag]
keepingIntermediatesOptions =
  [ flag { flagName = "-keep-hc-file, -keep-hc-files"
         , flagDescription = "Retain intermediate ``.hc`` files."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-keep-hi-files"
         , flagDescription =
           "Retain intermediate ``.hi`` files (the default)."
         , flagType = DynamicFlag
         , flagReverse = "-no-keep-hi-files"
         }
  , flag { flagName = "-keep-llvm-file, -keep-llvm-files"
         , flagDescription = "Retain intermediate LLVM ``.ll`` files. "++
           "Implies :ghc-flag:`-fllvm`."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-keep-o-files"
         , flagDescription =
           "Retain intermediate ``.o`` files (the default)."
         , flagType = DynamicFlag
         , flagReverse = "-no-keep-o-files"
         }
  , flag { flagName = "-keep-s-file, -keep-s-files"
         , flagDescription = "Retain intermediate ``.s`` files."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-keep-tmp-files"
         , flagDescription = "Retain all intermediate temporary files."
         , flagType = DynamicFlag
         }
  ]
