%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[System]{Module @System@}

\begin{code}
{-# OPTIONS -#include "cbits/stgio.h" #-}
module System ( 
    ExitCode(ExitSuccess,ExitFailure),
    getArgs, getProgName, getEnv, system, exitWith
  ) where

#ifdef __HUGS__
import PreludeBuiltin

indexAddrOffAddr = primIndexAddrOffAddr

unpackCString = unsafeUnpackCString

#else
import Prelude
import PrelAddr
import PrelIOBase	( IOError(..), IOErrorType(..), constructErrorAndFailWithInfo )
import PrelPack    	( unpackCString )
#endif

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


%*********************************************************
%*							*
\subsection{Other functions}
%*							*
%*********************************************************

\begin{code}
getArgs 		:: IO [String]
getProgName 		:: IO String
getEnv        		:: String -> IO String
system        		:: String -> IO ExitCode
exitWith   		:: ExitCode -> IO a
\end{code}

Computation $getArgs$ returns a list of the program's command
line arguments (not including the program name).

\begin{code}
#ifdef __HUGS__
foreign import stdcall "libHS_cbits.so" "get_prog_argv" primArgv :: Addr
foreign import stdcall "libHS_cbits.so" "get_prog_argc" primArgc :: Int

getArgs = return (unpackArgv primArgv primArgc)
#else
getArgs = return (unpackArgv ``prog_argv'' (``prog_argc''::Int))
#endif
\end{code}

Computation $getProgName$ returns the name of the program
as it was invoked.

\begin{code}
#ifdef __HUGS__
getProgName = return (unpackProgName primArgv)
#else
getProgName = return (unpackProgName ``prog_argv'')
#endif
\end{code}

Computation $getEnv var$ returns the value
of the environment variable {\em var}.  

This computation may fail with
\begin{itemize}
\item $NoSuchThing$
The environment variable does not exist.
\end{itemize}

\begin{code}
#ifdef __HUGS__
foreign import stdcall "libHS_cbits.so" "getenv" primGetEnv :: PrimByteArray -> IO Addr

getEnv name = do
    litstring <- primGetEnv (primPackString name)
    if litstring /= nullAddr
	then primUnpackCString litstring
        else fail (IOError Nothing NoSuchThing "getEnv"
			("environment variable: " ++ name))
#else
getEnv name = do
    litstring <- _ccall_ getenv name
    if litstring /= ``NULL'' 
	then return (unpackCString litstring)
        else fail (IOError Nothing NoSuchThing "getEnv"
			("environment variable: " ++ name))
#endif
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
#ifdef __HUGS__
foreign import stdcall "libHS_cbits.so" "systemCmd" primSystem :: PrimByteArray -> IO Int
system "" = fail (IOError Nothing InvalidArgument "system" "null command")
system cmd = do
    status <- primSystem (primPackString cmd)
    case status of
        0  -> return ExitSuccess
        -1 -> constructErrorAndFailWithInfo "system" cmd
        n  -> return (ExitFailure n)

#else
system "" = fail (IOError Nothing InvalidArgument "system" "null command")
system cmd = do
    status <- _ccall_ systemCmd cmd
    case status of
        0  -> return ExitSuccess
        -1 -> constructErrorAndFailWithInfo "system" cmd
        n  -> return (ExitFailure n)
#endif
\end{code}

Computation $exitWith code$ terminates the
program, returning {\em code} to the program's caller.
Before it terminates, any open or semi-closed handles are first closed.

\begin{code}
#ifdef __HUGS__
foreign import stdcall "libHS_cbits.so" "exit" primExit :: Int -> IO ()

exitWith ExitSuccess = do
    primExit 0
    fail (IOError Nothing OtherError "exitWith" "exit should not return")

exitWith (ExitFailure n) 
  | n == 0 = fail (IOError Nothing InvalidArgument "exitWith" "ExitFailure 0")
  | otherwise = do
    primExit n
    fail (IOError Nothing OtherError "exitWith" "exit should not return")
#else
exitWith ExitSuccess = do
    _ccall_ exit (0::Int)
    fail (IOError Nothing OtherError "exitWith" "exit should not return")

exitWith (ExitFailure n) 
  | n == 0 = fail (IOError Nothing InvalidArgument "exitWith" "ExitFailure 0")
  | otherwise = do
    _ccall_ exit n
    fail (IOError Nothing OtherError "exitWith" "exit should not return")
#endif
\end{code}


%*********************************************************
%*							*
\subsection{Local utilities}
%*							*
%*********************************************************

\begin{code}
type CHAR_STAR_STAR	= Addr	-- this is all a  HACK
type CHAR_STAR		= Addr

unpackArgv	:: CHAR_STAR_STAR -> Int -> [String] -- argv[1 .. argc-1]
unpackProgName	:: CHAR_STAR_STAR	 -> String   -- argv[0]

unpackArgv argv argc = unpack 1
  where
    unpack :: Int -> [String]
    unpack n
      = if (n >= argc)
	then ([] :: [String])
	else case (indexAddrOffAddr argv n) of { item ->
	     unpackCString item : unpack (n + 1) }

unpackProgName argv
  = case (indexAddrOffAddr argv 0) of { prog ->
    de_slash [] (unpackCString prog) }
  where
    -- re-start accumulating at every '/'
    de_slash :: String -> String -> String
    de_slash acc []	  = reverse acc
    de_slash acc ('/':xs) = de_slash []	     xs
    de_slash acc (x:xs)	  = de_slash (x:acc) xs
\end{code}
