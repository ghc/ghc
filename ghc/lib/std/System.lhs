%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[System]{Module @System@}

\begin{code}
module System ( 
    ExitCode(ExitSuccess,ExitFailure),
    getArgs, getProgName, getEnv, system, exitWith
  ) where

import Prelude
import PrelAddr
import PrelIOBase	( IOError(..), IOErrorType(..), constructErrorAndFail )
import PrelArr		( indexAddrOffAddr )
import PrelPack    	( unpackCString )

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
getArgs = return (unpackArgv ``prog_argv'' (``prog_argc''::Int))
\end{code}

Computation $getProgName$ returns the name of the program
as it was invoked.

\begin{code}
getProgName = return (unpackProgName ``prog_argv'')
\end{code}

Computation $getEnv var$ returns the value
of the environment variable {\em var}.  

This computation may fail with
\begin{itemize}
\item $NoSuchThing$
The environment variable does not exist.
\end{itemize}

\begin{code}
getEnv name = do
    litstring <- _ccall_ getenv name
    if litstring /= ``NULL'' 
	then return (unpackCString litstring)
        else fail (IOError Nothing NoSuchThing 
			("environment variable: " ++ name))
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
system "" = fail (IOError Nothing InvalidArgument "null command")
system cmd = do
    status <- _ccall_ systemCmd cmd
    case status of
        0  -> return ExitSuccess
        -1 -> constructErrorAndFail "system"
        n  -> return (ExitFailure n)

\end{code}

Computation $exitWith code$ terminates the
program, returning {\em code} to the program's caller.
Before it terminates, any open or semi-closed handles are first closed.

\begin{code}
exitWith ExitSuccess = do
    _ccall_ EXIT (0::Int)
    fail (IOError Nothing OtherError "exit should not return")

exitWith (ExitFailure n) 
  | n == 0 = fail (IOError Nothing InvalidArgument "ExitFailure 0")
  | otherwise = do
    _ccall_ EXIT n
    fail (IOError Nothing OtherError "exit should not return")
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
