{-
%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibSystem]{Haskell 1.3 System Interaction}
-}
module System ( 
    ExitCode(ExitSuccess,ExitFailure),
    getArgs, getProgName, getEnv, system, exitWith ) where

import GHCio
import GHCps	( unpackPS, packCString )
import GHCbase	( indexAddrOffAddr, Addr )

{-
The $ExitCode$ type defines the exit codes that a program
can return.  $ExitSuccess$ indicates successful termination;
and $ExitFailure code$ indicates program failure
with value {\em code}.  The exact interpretation of {\em code}
is operating-system dependent.  In particular, some values of 
{\em code} may be prohibited (e.g. 0 on a POSIX-compliant system).
-}

data ExitCode = ExitSuccess | ExitFailure Int 
                deriving (Eq, Ord, Read, Show)


getArgs 		:: IO [String]
getProgName 		:: IO String
getEnv        		:: String -> IO String
system        		:: String -> IO ExitCode
exitWith   		:: ExitCode -> IO a

{-
Computation $getArgs$ returns a list of the program's command
line arguments (not including the program name).
-}
getArgs = return (unpackArgv ``prog_argv'' (``prog_argc''::Int))

{-
Computation $getProgName$ returns the name of the program
as it was invoked.
-}
getProgName = return (unpackProgName ``prog_argv'')

{-
Computation $getEnv var$ returns the value
of the environment variable {\em var}.  

This computation may fail with
\begin{itemize}
\item $NoSuchThing$
The environment variable does not exist.
\end{itemize}
-}
getEnv name = 
    _ccall_ getenv name	`stThen` \ litstring ->
    if litstring /= ``NULL'' then
	return (unpackPS (packCString litstring)) -- cheaper than it looks
    else
	fail (NoSuchThing ("environment variable: " ++ name))

{-
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
-}
system "" = fail (InvalidArgument "null command")
system cmd = 
    _ccall_ systemCmd cmd	`stThen` \ status ->
    case status of
        0  -> return ExitSuccess
        -1 -> constructErrorAndFail "system"
        n  -> return (ExitFailure n)

{-
Computation $exitWith code$ terminates the
program, returning {\em code} to the program's caller.
Before it terminates, any open or semi-closed handles are first closed.
-}
exitWith ExitSuccess = 
    _ccall_ EXIT (0::Int)	`stThen` \ () ->
    fail (OtherError "exit should not return")

exitWith (ExitFailure n) 
  | n == 0 = fail (InvalidArgument "ExitFailure 0")
  | otherwise = 
    _ccall_ EXIT n		`stThen` \ () ->
    fail (OtherError "exit should not return")

------------------------------------------
-- like unpackCString ...

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
	     unpackPS (packCString item) : unpack (n + 1) }

unpackProgName argv
  = case (indexAddrOffAddr argv 0) of { prog ->
    de_slash [] (unpackPS (packCString prog)) }
  where
    -- re-start accumulating at every '/'
    de_slash :: String -> String -> String
    de_slash acc []	  = reverse acc
    de_slash acc ('/':xs) = de_slash []	     xs
    de_slash acc (x:xs)	  = de_slash (x:acc) xs
