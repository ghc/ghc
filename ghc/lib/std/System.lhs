-- -----------------------------------------------------------------------------
-- $Id: System.lhs,v 1.30 2001/05/18 16:54:05 simonmar Exp $
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
import PrelPtr
import PrelStorable
import PrelIOBase	( IOException(..), ioException, IOErrorType(..))

-- -----------------------------------------------------------------------------
-- The ExitCode type

-- The `ExitCode' type defines the exit codes that a program
-- can return.  `ExitSuccess' indicates successful termination;
-- and `ExitFailure code' indicates program failure
-- with value `code'.  The exact interpretation of `code'
-- is operating-system dependent.  In particular, some values of 
-- `code' may be prohibited (e.g. 0 on a POSIX-compliant system).

data ExitCode = ExitSuccess | ExitFailure Int 
                deriving (Eq, Ord, Read, Show)


-- Computation `getArgs' returns a list of the program's command
-- line arguments (not including the program name).

getArgs :: IO [String]
getArgs = unpackArgv primArgv primArgc

foreign import ccall "get_prog_argv" unsafe   primArgv :: Ptr (Ptr CChar)
foreign import ccall "get_prog_argc" unsafe   primArgc :: Int

-- Computation `getProgName' returns the name of the program
-- as it was invoked.

getProgName :: IO String
getProgName = unpackProgName primArgv

-- Computation `getEnv var' returns the value
-- of the environment variable {\em var}.  

-- This computation may fail with
--    NoSuchThing: The environment variable does not exist.

getEnv :: String -> IO String
getEnv name =
    withUnsafeCString name $ \s -> do
      litstring <- _getenv s
      if litstring /= nullPtr
	then peekCString litstring
        else ioException (IOError Nothing NoSuchThing "getEnv"
			  "no environment variable" (Just name))

foreign import ccall "getenv" unsafe _getenv :: UnsafeCString -> IO (Ptr CChar)

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
  withUnsafeCString cmd $ \s -> do
    status <- throwErrnoIfMinus1 "system" (primSystem s)
    case status of
        0  -> return ExitSuccess
        n  -> return (ExitFailure n)

foreign import ccall "systemCmd" unsafe primSystem :: UnsafeCString -> IO Int

-- ---------------------------------------------------------------------------
-- exitWith

-- `exitWith code' terminates the program, returning `code' to the
-- program's caller.  Before it terminates, any open or semi-closed
-- handles are first closed.

exitWith :: ExitCode -> IO a
exitWith ExitSuccess = do
    primExit 0
    ioException (IOError Nothing OtherError "exitWith" "exit should not return" Nothing)

exitWith (ExitFailure n) 
  | n == 0 = ioException (IOError Nothing InvalidArgument "exitWith" "ExitFailure 0" Nothing)
  | otherwise = do
    primExit n
    ioException (IOError Nothing OtherError "exitWith" "exit should not return" Nothing)

-- NOTE: shutdownHaskellAndExit must be called "safe", because it *can*
-- re-enter Haskell land through finalizers.
foreign import ccall "shutdownHaskellAndExit" primExit :: Int -> IO ()

exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)

-- ---------------------------------------------------------------------------
-- Local utilities

unpackArgv :: Ptr (Ptr CChar) -> Int -> IO [String] -- argv[1 .. argc-1]
unpackArgv argv argc
  = peekArray (argc-1) (advancePtr argv 1) >>= mapM peekCString

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
