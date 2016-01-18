module Options.Modes where

import Types

modeOptions :: [Flag]
modeOptions =
  [ flag { flagName = "--help,-?"
         , flagDescription = "Display help"
         , flagType = ModeFlag
         }
  , flag { flagName = "--interactive"
         , flagDescription =
           "Interactive mode - normally used by just running ``ghci``; "++
           "see :ref:`ghci` for details."
         , flagType = ModeFlag
         }
  , flag { flagName = "--make"
         , flagDescription =
           "Build a multi-module Haskell program, automatically figuring out "++
           "dependencies. Likely to be much easier, and faster, than using "++
           "``make``; see :ref:`make-mode` for details."
         , flagType = ModeFlag
         }
  , flag { flagName = "-e expr"
         , flagDescription =
           "Evaluate ``expr``; see :ref:`eval-mode` for details."
         , flagType = ModeFlag
         }
  , flag { flagName = "--show-iface"
         , flagDescription = "display the contents of an interface file."
         , flagType = ModeFlag
         }
  , flag { flagName = "-M"
         , flagDescription =
           "generate dependency information suitable for use in a "++
           "``Makefile``; see :ref:`makefile-dependencies` for details."
         , flagType = ModeFlag
         }
  , flag { flagName = "--frontend ⟨module⟩"
         , flagDescription =
           "run GHC with the given frontend plugin; see "++
           ":ref:`frontend_plugins` for details."
         , flagType = ModeFlag
         }
  , flag { flagName = "--supported-extensions, --supported-languages"
         , flagDescription = "display the supported language extensions"
         , flagType = ModeFlag
         }
  , flag { flagName = "--show-options"
         , flagDescription = "display the supported command line options"
         , flagType = ModeFlag
         }
  , flag { flagName = "--info"
         , flagDescription = "display information about the compiler"
         , flagType = ModeFlag
         }
  , flag { flagName = "--version, -V"
         , flagDescription = "display GHC version"
         , flagType = ModeFlag
         }
  , flag { flagName = "--numeric-version"
         , flagDescription = "display GHC version (numeric only)"
         , flagType = ModeFlag
         }
  , flag { flagName = "--print-libdir"
         , flagDescription = "display GHC library directory"
         , flagType = ModeFlag
         }
  ]
