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
    ( system        -- :: String -> IO ExitCode
    ) where

import Prelude

import System.Exit
import Foreign.C

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase
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
system :: String -> IO ExitCode
system "" = ioException (IOError Nothing InvalidArgument "system" "null command" Nothing)
system cmd =
  withCString cmd $ \s -> do
    status <- throwErrnoIfMinus1 "system" (primSystem s)
    case status of
        0  -> return ExitSuccess
        n  -> return (ExitFailure n)

foreign import ccall unsafe "systemCmd" primSystem :: CString -> IO Int
