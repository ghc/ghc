module Options.Linking where

import Types

linkingOptions :: [Flag]
linkingOptions =
  [ flag { flagName = "-shared"
         , flagDescription =
           "Generate a shared library (as opposed to an executable)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-staticlib"
         , flagDescription =
           "On Darwin/OS X/iOS only, generate a standalone static library " ++
           "(as opposed to an executable). This is the usual way to " ++
           "compile for iOS."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fPIC"
         , flagDescription =
           "Generate position-independent code (where available)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dynload"
         , flagDescription =
           "Selects one of a number of modes for finding shared libraries at runtime."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-framework⟨name⟩"
         , flagDescription =
           "On Darwin/OS X/iOS only, link in the framework ⟨name⟩. This " ++
           "option corresponds to the ``-framework`` option for Apple's Linker."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-framework-path⟨name⟩"
         , flagDescription =
           "On Darwin/OS X/iOS only, add ⟨dir⟩ to the list of directories " ++
           "searched for frameworks. This option corresponds to the ``-F`` "++
           "option for Apple's Linker."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-l⟨lib⟩"
         , flagDescription = "Link in library ⟨lib⟩"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-L⟨dir⟩"
         , flagDescription =
           "Add ⟨dir⟩ to the list of directories searched for libraries"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-main-is"
         , flagDescription = "Set main module and function"
         , flagType = DynamicFlag
         }
  , flag { flagName = "--mk-dll"
         , flagDescription = "DLL-creation mode (Windows only)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-no-hs-main"
         , flagDescription = "Don't assume this program contains ``main``"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-rtsopts,-rtsopts={none,some,all}"
         , flagDescription =
           "Control whether the RTS behaviour can be tweaked via command-line"++
           "flags and the ``GHCRTS`` environment variable. Using ``none`` " ++
           "means no RTS flags can be given; ``some`` means only a minimum " ++
           "of safe options can be given (the default), and ``all`` (or no " ++
           "argument at all) means that all RTS flags are permitted."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-with-rtsopts=opts"
         , flagDescription = "Set the default RTS options to ⟨opts⟩."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-no-rtsopts-suggestions"
         , flagDescription =
           "Don't print RTS suggestions about linking with :ghc-flag:`-rtsopts`."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-no-link"
         , flagDescription = "Omit linking"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-split-objs"
         , flagDescription = "Split objects (for libraries)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-split-sections"
         , flagDescription = "Split sections for link-time dead-code stripping"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-static"
         , flagDescription = "Use static Haskell libraries"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-threaded"
         , flagDescription = "Use the threaded runtime"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-debug"
         , flagDescription = "Use the debugging runtime"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-ticky"
         , flagDescription =
           "For linking, this simply implies :ghc-flag:`-debug`; "++
           "see :ref:`ticky-ticky`."
         , flagType = DynamicFlag
         }
  , flag { flagName = "-eventlog"
         , flagDescription = "Enable runtime event tracing"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fno-gen-manifest"
         , flagDescription = "Do not generate a manifest file (Windows only)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fno-embed-manifest"
         , flagDescription =
           "Do not embed the manifest in the executable (Windows only)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-fno-shared-implib"
         , flagDescription =
           "Don't generate an import library for a DLL (Windows only)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-dylib-install-name ⟨path⟩"
         , flagDescription =
           "Set the install name (via ``-install_name`` passed to Apple's " ++
           "linker), specifying the full install path of the library file. " ++
           "Any libraries or executables that link with it later will pick " ++
           "up that path as their runtime search location for it. " ++
           "(Darwin/OS X only)"
         , flagType = DynamicFlag
         }
  , flag { flagName = "-rdynamic"
         , flagDescription =
           "This instructs the linker to add all symbols, not only used " ++
           "ones, to the dynamic symbol table. Currently Linux and " ++
           "Windows/MinGW32 only. This is equivalent to using " ++
           "``-optl -rdynamic`` on Linux, and ``-optl -export-all-symbols`` " ++
           "on Windows."
         , flagType = DynamicFlag
         }
  ]
