{-# OPTIONS_GHC -cpp -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process.Internals
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Operations for creating and interacting with sub-processes.
--
-----------------------------------------------------------------------------

-- #hide
module System.Process.Internals (
	ProcessHandle(..), PHANDLE,
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
	 pPrPr_disableITimers, c_execvpe,
# ifdef __GLASGOW_HASKELL__
	runProcessPosix,
# endif
	ignoreSignal, defaultSignal,
#endif
	commandToProcess,
	withFilePathException, withCEnvironment
  ) where

import Prelude -- necessary to get dependencies right

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import System.Posix.Types ( CPid )
# ifdef __GLASGOW_HASKELL__
import GHC.IOBase	( haFD, FD, Exception(..), IOException(..) )
import GHC.Handle 	( stdin, stdout, stderr, withHandle_ )
# elif __HUGS__
import Hugs.Exception	( Exception(..), IOException(..) )
# endif
import System.IO 	( Handle )
import Data.Maybe	( fromMaybe )
#else
import Data.Word ( Word32 )
#endif

import Control.Exception ( handle, throwIO )
import Foreign.C
import Foreign

#ifdef __HUGS__
{-# CFILES cbits/execvpe.c  #-}
#endif

#include "HsBaseConfig.h"

-- ----------------------------------------------------------------------------
-- ProcessHandle type

{- | A handle to a process, which can be used to wait for termination
     of the process using 'waitForProcess'.

     None of the process-creation functions in this library wait for
     termination: they all return a 'ProcessHandle' which may be used
     to wait for the process later.
-}
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
type PHANDLE = CPid
#else
type PHANDLE = Word32
#endif

newtype ProcessHandle = ProcessHandle PHANDLE

-- ----------------------------------------------------------------------------

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

-- this function disables the itimer, which would otherwise cause confusing
-- signals to be sent to the new process.
foreign import ccall unsafe "pPrPr_disableITimers"
  pPrPr_disableITimers :: IO ()

foreign import ccall unsafe "execvpe"
  c_execvpe :: CString -> Ptr CString -> Ptr CString -> IO CInt

#endif

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

#ifdef __GLASGOW_HASKELL__
-- -----------------------------------------------------------------------------
-- POSIX runProcess with signal handling in the child

runProcessPosix
  :: String
  -> FilePath			-- ^ Filename of the executable
  -> [String]			-- ^ Arguments to pass to the executable
  -> Maybe FilePath		-- ^ Optional path to the working directory
  -> Maybe [(String,String)]	-- ^ Optional environment (otherwise inherit)
  -> Maybe Handle		-- ^ Handle to use for @stdin@
  -> Maybe Handle		-- ^ Handle to use for @stdout@
  -> Maybe Handle		-- ^ Handle to use for @stderr@
  -> Maybe CLong		-- handler for SIGINT
  -> Maybe CLong		-- handler for SIGQUIT
  -> IO ProcessHandle

runProcessPosix fun cmd args mb_cwd mb_env mb_stdin mb_stdout mb_stderr
	mb_sigint mb_sigquit
 = withFilePathException cmd $
     withHandle_ fun (fromMaybe stdin  mb_stdin)  $ \hndStdInput  ->
     withHandle_ fun (fromMaybe stdout mb_stdout) $ \hndStdOutput ->
     withHandle_ fun (fromMaybe stderr mb_stderr) $ \hndStdError ->
     maybeWith withCEnvironment mb_env $ \pEnv ->
     maybeWith withCString mb_cwd $ \pWorkDir ->
     withMany withCString (cmd:args) $ \cstrs ->
     let (set_int, inthand) 
		= case mb_sigint of
			Nothing   -> (0, 0)
			Just hand -> (1, hand)
	 (set_quit, quithand) 
		= case mb_sigquit of
			Nothing   -> (0, 0)
			Just hand -> (1, hand)
     in
     withArray0 nullPtr cstrs $ \pargs -> do
         ph <- throwErrnoIfMinus1 fun $
		 c_runProcess pargs pWorkDir pEnv 
			(haFD hndStdInput)
			(haFD hndStdOutput)
			(haFD hndStdError)
			set_int inthand set_quit quithand
	 return (ProcessHandle ph)

foreign import ccall unsafe "runProcess" 
  c_runProcess
        :: Ptr CString			-- args
        -> CString			-- working directory (or NULL)
        -> Ptr CString			-- env (or NULL)
        -> FD				-- stdin
        -> FD				-- stdout
        -> FD				-- stderr
	-> CInt				-- non-zero: set child's SIGINT handler
	-> CLong			-- SIGINT handler
	-> CInt				-- non-zero: set child's SIGQUIT handler
	-> CLong			-- SIGQUIT handler
        -> IO PHANDLE

#endif /* __GLASGOW_HASKELL__ */

ignoreSignal  = CONST_SIG_IGN :: CLong
defaultSignal = CONST_SIG_DFL :: CLong

#endif

-- ----------------------------------------------------------------------------
-- commandToProcess

{- | Turns a shell command into a raw command.  Usually this involves
     wrapping it in an invocation of the shell.

   There's a difference in the signature of commandToProcess between
   the Windows and Unix versions.  On Unix, exec takes a list of strings,
   and we want to pass our command to /bin/sh as a single argument.  

   On Windows, CreateProcess takes a single string for the command,
   which is later decomposed by cmd.exe.  In this case, we just want
   to prepend @\"c:\WINDOWS\CMD.EXE \/c\"@ to our command line.  The
   command-line translation that we normally do for arguments on
   Windows isn't required (or desirable) here.
-}

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

commandToProcess
  :: String
  -> IO (FilePath,[String])
commandToProcess string = return ("/bin/sh", ["-c", string])

#else

commandToProcess
  :: String
  -> IO (FilePath,String)
commandToProcess string = do
  sysDir <- allocaBytes 1024 (\pdir -> c_getSystemDirectory pdir 1024 >> peekCString pdir)
  return (sysDir ++ "\\CMD.EXE", "/c " ++ string)
	-- We don't want to put the cmd into a single
	-- argument, because cmd.exe will not try to split it up.  Instead,
	-- we just tack the command on the end of the cmd.exe command line,
	-- which partly works.  There seem to be some quoting issues, but
	-- I don't have the energy to find+fix them right now (ToDo). --SDM

foreign import stdcall unsafe "GetSystemDirectoryA" 
  c_getSystemDirectory 
        :: CString 
        -> CInt 
        -> IO CInt

#endif

-- ----------------------------------------------------------------------------
-- Utils

withFilePathException :: FilePath -> IO a -> IO a
withFilePathException fpath act = handle mapEx act
  where
    mapEx (IOException (IOError h iot fun str _)) = ioError (IOError h iot fun str (Just fpath))
    mapEx e                                       = throwIO e

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
withCEnvironment :: [(String,String)] -> (Ptr CString  -> IO a) -> IO a
withCEnvironment env act =
  let env' = map (\(name, val) -> name ++ ('=':val)) env 
  in withMany withCString env' (\pEnv -> withArray0 nullPtr pEnv act)
#else
withCEnvironment :: [(String,String)] -> (Ptr () -> IO a) -> IO a
withCEnvironment env act =
  let env' = foldr (\(name, val) env -> name ++ ('=':val)++'\0':env) "\0" env 
  in withCString env' (act . castPtr)
#endif

