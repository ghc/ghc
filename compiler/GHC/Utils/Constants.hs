{-# LANGUAGE CPP #-}

module GHC.Utils.Constants
  ( debugIsOn
  , internalInterpreterSupported
  , isWindowsHost
  , isDarwinHost
  )
where

import GHC.Prelude.Basic

{-

These booleans are global constants, set by CPP flags.  They allow us to
recompile a single module (this one) to change whether or not debug output
appears. They sometimes let us avoid even running CPP elsewhere.

It's important that the flags are literal constants (True/False). Then,
with -0, tests of the flags in other modules will simplify to the correct
branch of the conditional, thereby dropping debug code altogether when
the flags are off.
-}

internalInterpreterSupported :: Bool
#if defined(HAVE_INTERNAL_INTERPRETER)
internalInterpreterSupported = True
#else
internalInterpreterSupported = False
#endif

debugIsOn :: Bool
#if defined(DEBUG)
debugIsOn = True
#else
debugIsOn = False
#endif

isWindowsHost :: Bool
#if defined(mingw32_HOST_OS)
isWindowsHost = True
#else
isWindowsHost = False
#endif

isDarwinHost :: Bool
#if defined(darwin_HOST_OS)
isDarwinHost = True
#else
isDarwinHost = False
#endif
