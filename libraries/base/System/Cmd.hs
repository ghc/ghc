-----------------------------------------------------------------------------
-- 
-- Module      :  System.Cmd
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Cmd.hs,v 1.1 2002/02/06 10:14:26 simonmar Exp $
--
-- Executing a command.
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

-- Computation `system cmd' returns the exit code
-- produced when the operating system processes the command `cmd'.

-- This computation may fail with
--   PermissionDenied 
--	The process has insufficient privileges to perform the operation.
--   ResourceExhausted
--      Insufficient resources are available to perform the operation.  
--   UnsupportedOperation
--	The implementation does not support system calls.

system :: String -> IO ExitCode
system "" = ioException (IOError Nothing InvalidArgument "system" "null command" Nothing)
system cmd =
  withCString cmd $ \s -> do
    status <- throwErrnoIfMinus1 "system" (primSystem s)
    case status of
        0  -> return ExitSuccess
        n  -> return (ExitFailure n)

foreign import ccall "systemCmd" unsafe primSystem :: CString -> IO Int
