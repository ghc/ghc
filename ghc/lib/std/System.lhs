% -----------------------------------------------------------------------------
% $Id: System.lhs,v 1.29 2001/01/11 17:51:02 simonmar Exp $
%
% (c) The University of Glasgow, 1994-2000
%

\section[System]{Module @System@}

\begin{code}
{-# OPTIONS -#include "cbits/stgio.h" #-}
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
\end{code}

\begin{code}
import Monad
import Prelude
import PrelCString
import PrelCTypes
import PrelMarshalArray
import PrelPtr
import PrelStorable
import PrelIOBase	( IOException(..), ioException, 
			  IOErrorType(..), constructErrorAndFailWithInfo )
\end{code}

%*********************************************************
%*							*
\subsection{The @ExitCode@ type}
%*							*
%*********************************************************

The $ExitCode$ type defines the exit codes that a program
can return.  $ExitSuccess$ indicates successful termination;
and $ExitFailure code$ indicates program failure
with value {\em code}.  The exact interpretation of {\em code}
is operating-system dependent.  In particular, some values of 
{\em code} may be prohibited (e.g. 0 on a POSIX-compliant system).

\begin{code}
data ExitCode = ExitSuccess | ExitFailure Int 
                deriving (Eq, Ord, Read, Show)

\end{code}

Computation $getArgs$ returns a list of the program's command
line arguments (not including the program name).

\begin{code}
getArgs :: IO [String]
getArgs = unpackArgv primArgv primArgc

foreign import ccall "get_prog_argv" unsafe   primArgv :: Ptr (Ptr CChar)
foreign import ccall "get_prog_argc" unsafe   primArgc :: Int
\end{code}

Computation $getProgName$ returns the name of the program
as it was invoked.

\begin{code}
getProgName :: IO String
getProgName = unpackProgName primArgv
\end{code}

Computation $getEnv var$ returns the value
of the environment variable {\em var}.  

This computation may fail with
\begin{itemize}
\item $NoSuchThing$
The environment variable does not exist.
\end{itemize}

\begin{code}
getEnv :: String -> IO String
getEnv name =
    withUnsafeCString name $ \s -> do
      litstring <- _getenv s
      if litstring /= nullPtr
	then peekCString litstring
        else ioException (IOError Nothing NoSuchThing "getEnv"
			  "no environment variable" (Just name))

foreign import ccall "getenv" unsafe _getenv :: UnsafeCString -> IO (Ptr CChar)
\end{code}

Computation $system cmd$ returns the exit code
produced when the operating system processes the command {\em cmd}.

This computation may fail with
\begin{itemize}
\item $PermissionDenied$
The process has insufficient privileges to perform the operation.
\item $ResourceExhausted$
Insufficient resources are available to perform the operation.  
\item $UnsupportedOperation$
The implementation does not support system calls.
\end{itemize}

\begin{code}
system        		:: String -> IO ExitCode
system "" = ioException (IOError Nothing InvalidArgument "system" "null command" Nothing)
system cmd =
  withUnsafeCString cmd $ \s -> do
    status <- primSystem s
    case status of
        0  -> return ExitSuccess
        -1 -> constructErrorAndFailWithInfo "system" cmd
        n  -> return (ExitFailure n)

foreign import ccall "systemCmd" unsafe primSystem :: UnsafeCString -> IO Int
\end{code}

@exitWith code@ terminates the program, returning {\em code} to the program's caller.
Before it terminates, any open or semi-closed handles are first closed.

\begin{code}
exitWith   		:: ExitCode -> IO a
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
\end{code}


%*********************************************************
%*							*
\subsection{Local utilities}
%*							*
%*********************************************************

\begin{code}
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
