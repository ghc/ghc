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
      rawSystem,     -- :: FilePath -> [String] -> IO ExitCode
    ) where

import Prelude

import System.Exit 	( ExitCode )

#ifdef __GLASGOW_HASKELL__
import System.Process
import GHC.IOBase	( ioException, IOException(..), IOErrorType(..) )
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
system cmd = do
  p <- runCommand cmd
  waitForProcess p
#endif  /* __GLASGOW_HASKELL__ */

{-|
The computation @'rawSystem' cmd args@ runs the operating system command
@cmd@ in such a way that it receives as arguments the @args@ strings
exactly as given, with no funny escaping or shell meta-syntax expansion.
It will therefore behave more portably between operating systems than 'system'.

The return codes and possible failures are the same as for 'system'.
-}
rawSystem :: String -> [String] -> IO ExitCode
#ifdef __GLASGOW_HASKELL__
rawSystem cmd args = do
  p <- runProcess cmd args Nothing Nothing Nothing Nothing Nothing
  waitForProcess p
#else /* ! __GLASGOW_HASKELL__ */
-- crude fallback implementation: could do much better than this under Unix
rawSystem cmd args = system (unwords (map translate (cmd:args)))

translate :: String -> String
#if defined(mingw32_HOST_OS)
-- copied from System.Process (qv)
translate str = '"' : snd (foldr escape (True,"\"") str)
  where	escape '"'  (b,     str) = (True,  '\\' : '"'  : str)
	escape '\\' (True,  str) = (True,  '\\' : '\\' : str)
	escape '\\' (False, str) = (False, '\\' : str)
	escape c    (b,     str) = (False, c : str)
#else /* ! mingw32_HOST_OS */
translate str = '\'' : foldr escape "'" str
  where	escape '\'' cs = '\'' : '\\' : '\'' : '\'' : cs
	escape c    cs = c : cs
#endif /* ! mingw32_HOST_OS */
#endif /* ! __GLASGOW_HASKELL__ */
