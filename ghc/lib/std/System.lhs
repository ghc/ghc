-- -----------------------------------------------------------------------------
-- $Id: System.lhs,v 1.33 2001/08/14 17:14:22 sof Exp $
--
-- (c) The University of Glasgow, 1994-2000
--

\begin{code}
module System 
    ( 
      ExitCode(ExitSuccess,ExitFailure)
    , getArgs	    -- :: IO [String]
    , getProgName   -- :: IO String
    , getEnv        -- :: String -> IO String
    , system        -- :: String -> IO ExitCode
    , exitWith      -- :: ExitCode -> IO a
    , exitFailure   -- :: IO a
  ) where

import Monad
import Prelude
import PrelCError
import PrelCString
import PrelCTypes
import PrelMarshalArray
import PrelMarshalAlloc
import PrelPtr
import PrelStorable
import PrelIOBase
import PrelConc

-- ---------------------------------------------------------------------------
-- getArgs, getProgName, getEnv

-- Computation `getArgs' returns a list of the program's command
-- line arguments (not including the program name).

getArgs :: IO [String]
getArgs = 
  alloca $ \ p_argc ->  
  alloca $ \ p_argv -> do
   getProgArgv p_argc p_argv
   p    <- peek p_argc
   argv <- peek p_argv
   peekArray (p - 1) (advancePtr argv 1) >>= mapM peekCString
   
   
foreign import "getProgArgv" getProgArgv :: Ptr Int -> Ptr (Ptr CString) -> IO ()

-- Computation `getProgName' returns the name of the program
-- as it was invoked.

getProgName :: IO String
getProgName = 
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
     getProgArgv p_argc p_argv
     argv <- peek p_argv
     unpackProgName argv

-- Computation `getEnv var' returns the value
-- of the environment variable {\em var}.  

-- This computation may fail with
--    NoSuchThing: The environment variable does not exist.

getEnv :: String -> IO String
getEnv name =
    withCString name $ \s -> do
      litstring <- _getenv s
      if litstring /= nullPtr
	then peekCString litstring
        else ioException (IOError Nothing NoSuchThing "getEnv"
			  "no environment variable" (Just name))

foreign import ccall "getenv" unsafe _getenv :: CString -> IO (Ptr CChar)

-- ---------------------------------------------------------------------------
-- system

-- Computation `system cmd' returns the exit code
-- produced when the operating system processes the command {\em cmd}.

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

-- ---------------------------------------------------------------------------
-- exitWith

-- `exitWith code' terminates the program, returning `code' to the
-- program's caller.  Before it terminates, any open or semi-closed
-- handles are first closed.

exitWith :: ExitCode -> IO a
exitWith ExitSuccess = throw (ExitException ExitSuccess)
exitWith code@(ExitFailure n) 
  | n == 0 = ioException (IOError Nothing InvalidArgument "exitWith" "ExitFailure 0" Nothing)
  | otherwise = throw (ExitException code)

exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)

-- ---------------------------------------------------------------------------
-- Local utilities

unpackProgName	:: Ptr (Ptr CChar) -> IO String   -- argv[0]
unpackProgName argv = do 
  s <- peekElemOff argv 0 >>= peekCString
  return (de_slash "" s)
  where
    -- re-start accumulating at every '/'
    de_slash :: String -> String -> String
    de_slash  acc []	   = reverse acc
    de_slash _acc ('/':xs) = de_slash []      xs
    de_slash  acc (x:xs)   = de_slash (x:acc) xs
\end{code}
