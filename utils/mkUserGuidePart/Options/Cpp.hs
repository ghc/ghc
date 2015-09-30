module Options.Cpp where

import Types

cppOptions :: [Flag]
cppOptions =
  [ flag { flagName = "-cpp"
         , flagDescription = "Run the C pre-processor on Haskell source files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-D⟨symbol⟩[=⟨value⟩]"
         , flagDescription = "Define a symbol in the C pre-processor"
         , flagType = DynamicFlag
         , flagReverse = "-U⟨symbol⟩"
         }
  , flag { flagName = "-U⟨symbol⟩"
         , flagDescription = "Undefine a symbol in the C pre-processor"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-I⟨dir⟩"
         , flagDescription =
           "Add ⟨dir⟩ to the directory search list for ``#include`` files"
         , flagType = DynamicFlag
         }
  ]
