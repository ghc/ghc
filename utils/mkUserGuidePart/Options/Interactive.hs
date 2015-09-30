module Options.Interactive where

import Types

interactiveOptions :: [Flag]
interactiveOptions =
  [ flag { flagName = "-ignore-dot-ghci"
         , flagDescription = "Disable reading of ``.ghci`` files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ghci-script"
         , flagDescription = "Read additional ``.ghci`` files"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fbreak-on-error"
         , flagDescription =
           ":ref:`Break on uncaught exceptions and errors " ++
           "<ghci-debugger-exceptions>`"
         , flagType = DynamicFlag
         , flagReverse = "-fno-break-on-error"
         }
  , flag { flagName = "-fbreak-on-exception"
         , flagDescription =
           ":ref:`Break on any exception thrown <ghci-debugger-exceptions>`"
         , flagType = DynamicFlag
         , flagReverse = "-fno-break-on-exception"
         }
  , flag { flagName = "-fghci-hist-size=⟨n⟩"
         , flagDescription =
           "Set the number of entries GHCi keeps for ``:history``." ++
           " See :ref:`ghci-debugger`."
         , flagType = DynamicFlag
         , flagReverse = "(default is 50)"
         }
  , flag { flagName = "-fprint-evld-with-show"
         , flagDescription =
           "Enable usage of ``Show`` instances in ``:print``. "++
           "See :ref:`breakpoints`."
         , flagType = DynamicFlag
         , flagReverse = "-fno-print-evld-with-show"
         }
  , flag { flagName = "-fprint-bind-result"
         , flagDescription =
           ":ref:`Turn on printing of binding results in GHCi <ghci-stmts>`"
         , flagType = DynamicFlag
         , flagReverse = "-fno-print-bind-result"
         }
  , flag { flagName = "-fno-print-bind-contents"
         , flagDescription =
           ":ref:`Turn off printing of binding contents in GHCi <breakpoints>`"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fno-implicit-import-qualified"
         , flagDescription =
           ":ref:`Turn off implicit qualified import of everything in GHCi " ++
           "<ghci-import-qualified>`"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-interactive-print"
         , flagDescription =
           ":ref:`Select the function to use for printing evaluated " ++
           "expressions in GHCi <ghci-interactive-print>`"
         , flagType = DynamicFlag
         }
  ]
