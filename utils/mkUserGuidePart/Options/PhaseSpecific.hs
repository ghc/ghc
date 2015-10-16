module Options.PhaseSpecific where

import Types

phaseSpecificOptions :: [Flag]
phaseSpecificOptions =
  [ flag { flagName = "-optL⟨option⟩"
         , flagDescription = "pass ⟨option⟩ to the literate pre-processor"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-optP⟨option⟩"
         , flagDescription = "pass ⟨option⟩ to cpp (with ``-cpp`` only)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-optF⟨option⟩"
         , flagDescription = "pass ⟨option⟩ to the custom pre-processor"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-optc⟨option⟩"
         , flagDescription = "pass ⟨option⟩ to the C compiler"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-optlo⟨option⟩"
         , flagDescription = "pass ⟨option⟩ to the LLVM optimiser"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-optlc⟨option⟩"
         , flagDescription = "pass ⟨option⟩ to the LLVM compiler"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-opta⟨option⟩"
         , flagDescription = "pass ⟨option⟩ to the assembler"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-optl⟨option⟩"
         , flagDescription = "pass ⟨option⟩ to the linker"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-optdll⟨option⟩"
         , flagDescription = "pass ⟨option⟩ to the DLL generator"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-optwindres⟨option⟩"
         , flagDescription = "pass ⟨option⟩ to ``windres``."
         , flagType = DynamicFlag
         }
  ]
