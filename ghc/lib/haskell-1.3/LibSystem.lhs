%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibSystem]{Haskell 1.3 System Interaction}

\begin{code}
module LibSystem where

import PreludeGlaST
import PreludeIOError
import PreludeDialogueIO  ( unpackArgv, unpackProgName )

data ExitCode = ExitSuccess 
	      | ExitFailure Int
{- mattson -} deriving (Eq, Ord, Text)

\end{code}

The $ExitCode$ type defines the exit codes that a program
can return.  $ExitSuccess$ indicates successful termination;
and $ExitFailure code$ indicates program failure
with value {\em code}.  The exact interpretation of {\em code}
is operating-system dependent.  In particular, some values of 
{\em code} may be prohibited (e.g. 0 on a POSIX-compliant system).

\begin{code}
getArgs :: IO [String] 
getArgs = return (unpackArgv ``prog_argv'' (``prog_argc''::Int))
\end{code}

Computation $getArgs$ returns a list of the program's command
line arguments (not including the program name).

\begin{code}
getProgName :: IO String
getProgName = return (unpackProgName ``prog_argv'')
\end{code}

Computation $getProgName$ returns the name of the program
as it was invoked.

\begin{code}
getEnv :: String -> IO String
getEnv name = 
    _ccall_ getenv name				    `thenPrimIO` \ litstring ->
    if litstring /= ``NULL'' then
	return (_unpackPS (_packCString litstring)) -- cheaper than it looks
    else
	failWith (NoSuchThing ("environment variable: " ++ name))
\end{code}

Computation $getEnv var$ returns the value
of the environment variable {\em var}.  

This computation may fail with
\begin{itemize}
\item $NoSuchThing$
The environment variable does not exist.
\end{itemize}

\begin{code}
system :: String -> IO ExitCode
system "" = failWith (InvalidArgument "null command")
system cmd = 
    _ccall_ systemCmd cmd			    `thenPrimIO` \ status ->
    case status of
        0  -> return ExitSuccess
        -1 -> _constructError			    `thenPrimIO` \ ioError ->
	      failWith ioError
        n  -> return (ExitFailure n)
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
exitWith :: ExitCode -> IO a
exitWith ExitSuccess = 
    _ccall_ EXIT (0::Int)			    `thenPrimIO` \ () ->
    failWith (OtherError13 "exit should not return")

exitWith (ExitFailure n) 
  | n == 0 = failWith (InvalidArgument "ExitFailure 0")
  | otherwise = 
    _ccall_ EXIT n				    `thenPrimIO` \ () ->
    failWith (OtherError13 "exit should not return")
\end{code}

Computation $exitWith code$ terminates the
program, returning {\em code} to the program's caller.
Before it terminates, any open or semi-closed handles are first closed.


