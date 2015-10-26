module Options.CodeGen where

import Types

codegenOptions :: [Flag]
codegenOptions =
  [ flag { flagName = "-fasm"
         , flagDescription =
           "Use the :ref:`native code generator <native-code-gen>`"
         , flagType = DynamicFlag
         , flagReverse = "-fllvm"
         }
  , flag { flagName = "-fllvm"
         , flagDescription =
           "Compile using the :ref:`LLVM code generator <llvm-code-gen>`"
         , flagType = DynamicFlag
         , flagReverse = "-fasm"
         }
  , flag { flagName = "-fno-code"
         , flagDescription = "Omit code generation"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fwrite-interface"
         , flagDescription = "Always write interface files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fbyte-code"
         , flagDescription = "Generate byte-code"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fobject-code"
         , flagDescription = "Generate object code"
         , flagType = DynamicFlag
         }
  ]
