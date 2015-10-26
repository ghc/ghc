module Options.PhasePrograms where

import Types

phaseProgramsOptions :: [Flag]
phaseProgramsOptions =
  [ flag { flagName = "-pgmL⟨cmd⟩"
         , flagDescription = "Use ⟨cmd⟩ as the literate pre-processor"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-pgmP⟨cmd⟩"
         , flagDescription =
           "Use ⟨cmd⟩ as the C pre-processor (with ``-cpp`` only)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-pgmc⟨cmd⟩"
         , flagDescription = "Use ⟨cmd⟩ as the C compiler"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-pgmlo⟨cmd⟩"
         , flagDescription = "Use ⟨cmd⟩ as the LLVM optimiser"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-pgmlc⟨cmd⟩"
         , flagDescription = "Use ⟨cmd⟩ as the LLVM compiler"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-pgms⟨cmd⟩"
         , flagDescription = "Use ⟨cmd⟩ as the splitter"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-pgma⟨cmd⟩"
         , flagDescription = "Use ⟨cmd⟩ as the assembler"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-pgml⟨cmd⟩"
         , flagDescription = "Use ⟨cmd⟩ as the linker"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-pgmdll⟨cmd⟩"
         , flagDescription = "Use ⟨cmd⟩ as the DLL generator"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-pgmF⟨cmd⟩"
         , flagDescription = "Use ⟨cmd⟩ as the pre-processor (with ``-F`` only)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-pgmwindres⟨cmd⟩"
         , flagDescription =
           "Use ⟨cmd⟩ as the program for embedding manifests on Windows."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-pgmlibtool⟨cmd⟩"
         , flagDescription =
           "Use ⟨cmd⟩ as the command for libtool (with ``-staticlib`` only)."
         , flagType = DynamicFlag
         }
  ]
