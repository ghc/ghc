-----------------------------------------------------------------------------
-- |
-- Module      :  System.Cmd
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Executing an external command.
--
-----------------------------------------------------------------------------

module System.Cmd
    ( system,        -- :: String -> IO ExitCode
#ifdef __GLASGOW_HASKELL__
      rawSystem,     -- :: FilePath -> [String] -> IO ExitCode
#endif
    ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import Foreign
import Foreign.C
import System.Exit
import GHC.IOBase
#include "config.h"
#endif

#ifdef __HUGS__
import Hugs.System
#endif

#ifdef __NHC__
import System (system)
#endif

-- ---------------------------------------------------------------------------
-- system

{-| 
Computation @system cmd@ returns the exit code
produced when the operating system processes the command @cmd@.

This computation may fail with

   * @PermissionDenied@: The process has insufficient privileges to
     perform the operation.

   * @ResourceExhausted@: Insufficient resources are available to
     perform the operation.

   * @UnsupportedOperation@: The implementation does not support
     system calls.

On Windows, 'system' is implemented using Windows's native system
call, which ignores the @SHELL@ environment variable, and always
passes the command to the Windows command interpreter (@CMD.EXE@ or
@COMMAND.COM@), hence Unixy shell tricks will not work.
-}
#ifdef __GLASGOW_HASKELL__
system :: String -> IO ExitCode
system "" = ioException (IOError Nothing InvalidArgument "system" "null command" Nothing)
system cmd =
  withCString cmd $ \s -> do
    status <- throwErrnoIfMinus1 "system" (primSystem s)
    case status of
        0  -> return ExitSuccess
        n  -> return (ExitFailure n)

foreign import ccall unsafe "systemCmd" primSystem :: CString -> IO Int
#endif  /* __GLASGOW_HASKELL__ */

-- ---------------------------------------------------------------------------
-- rawSystem

-- rawSystem is in a separate file, so we can #include it various places.
#include "RawSystem.hs-inc"
