%
% (c) The AQUA Project, Glasgow University, 1994-1999
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


#ifndef __HUGS__
\begin{code}
import Prelude
import PrelAddr
import PrelIOBase	( IOError(..), IOErrorType(..), constructErrorAndFailWithInfo, stToIO )
import PrelPack    	( unpackCString, unpackCStringST, packString )
import PrelByteArr	( ByteArray )

type PrimByteArray  = ByteArray Int

primUnpackCString :: Addr -> IO String
primUnpackCString s = stToIO ( unpackCStringST s )

primPackString :: String -> PrimByteArray
primPackString s    = packString s

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
getArgs 		:: IO [String]
getArgs = return (unpackArgv primArgv primArgc)

foreign import ccall "libHS_cbits.so" "get_prog_argv" unsafe primArgv :: Addr
foreign import ccall "libHS_cbits.so" "get_prog_argc" unsafe primArgc :: Int
\end{code}

Computation $getProgName$ returns the name of the program
as it was invoked.

\begin{code}
getProgName 		:: IO String
getProgName = return (unpackProgName primArgv)
\end{code}

Computation $getEnv var$ returns the value
of the environment variable {\em var}.  

This computation may fail with
\begin{itemize}
\item $NoSuchThing$
The environment variable does not exist.
\end{itemize}

\begin{code}
getEnv        		:: String -> IO String
getEnv name = do
    litstring <- primGetEnv (primPackString name)
    if litstring /= nullAddr
	then primUnpackCString litstring
        else ioError (IOError Nothing NoSuchThing "getEnv"
			("environment variable: " ++ name))

foreign import ccall "libHS_cbits.so" "getenv" unsafe primGetEnv :: PrimByteArray -> IO Addr
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
system "" = ioError (IOError Nothing InvalidArgument "system" "null command")
system cmd = do
    status <- primSystem (primPackString cmd)
    case status of
        0  -> return ExitSuccess
        -1 -> constructErrorAndFailWithInfo "system" cmd
        n  -> return (ExitFailure n)

foreign import ccall "libHS_cbits.so" "systemCmd" unsafe primSystem :: PrimByteArray -> IO Int
\end{code}

@exitWith code@ terminates the program, returning {\em code} to the program's caller.
Before it terminates, any open or semi-closed handles are first closed.

\begin{code}
exitWith   		:: ExitCode -> IO a
exitWith ExitSuccess = do
    primExit 0
    ioError (IOError Nothing OtherError "exitWith" "exit should not return")

exitWith (ExitFailure n) 
  | n == 0 = ioError (IOError Nothing InvalidArgument "exitWith" "ExitFailure 0")
  | otherwise = do
    primExit n
    ioError (IOError Nothing OtherError "exitWith" "exit should not return")

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
type CHAR_STAR_STAR	= Addr	-- this is all a  HACK
type CHAR_STAR		= Addr

unpackArgv :: CHAR_STAR_STAR -> Int -> [String] -- argv[1 .. argc-1]
unpackArgv argv argc = unpack 1
  where
   unpack :: Int -> [String]
   unpack n
     | n >= argc = []
     | otherwise =
	 case (indexAddrOffAddr argv n) of 
	   item -> unpackCString item : unpack (n + 1)

unpackProgName	:: CHAR_STAR_STAR	 -> String   -- argv[0]
unpackProgName argv
  = case (indexAddrOffAddr argv 0) of { prog ->
    de_slash [] (unpackCString prog) }
  where
    -- re-start accumulating at every '/'
    de_slash :: String -> String -> String
    de_slash  acc []	   = reverse acc
    de_slash _acc ('/':xs) = de_slash []      xs
    de_slash  acc (x:xs)   = de_slash (x:acc) xs
\end{code}

#else

\begin{code}
-----------------------------------------------------------------------------
-- Standard Library: System operations
--
-- Warning: the implementation of these functions in Hugs 98 is very weak.
-- The functions themselves are best suited to uses in compiled programs,
-- and not to use in an interpreter-based environment like Hugs.
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------
import Prelude
import privileged Prelude ( primGetRawArgs
			  , primGetEnv
			  , prelCleanupAfterRunAction
			  , copy_String_to_cstring
			  , readIORef
			  , nh_stderr
			  , nh_stdout
			  , nh_stdin 
			  , nh_exitwith 
			  , nh_flush
			  , nh_close
			  , nh_system
			  , nh_free
			  , nh_getPID
			  )


data ExitCode = ExitSuccess | ExitFailure Int
                deriving (Eq, Ord, Read, Show)

getArgs                     :: IO [String]
getArgs                      = primGetRawArgs >>= \rawargs ->
                               return (drop 1 (dropWhile (/= "--") rawargs))

getProgName                 :: IO String
getProgName                  = primGetRawArgs >>= \rawargs ->
                               return (head rawargs)

getEnv                      :: String -> IO String
getEnv                       = primGetEnv

exitFailure		    :: IO a
exitFailure		     = exitWith (ExitFailure 1)

toExitCode                  :: Int -> ExitCode
toExitCode 0                 = ExitSuccess
toExitCode n                 = ExitFailure n

fromExitCode                :: ExitCode -> Int
fromExitCode ExitSuccess     = 0
fromExitCode (ExitFailure n) = n

-- see comment in Prelude.hs near primRunIO_hugs_toplevel
exitWith :: ExitCode -> IO a
exitWith c
   = do cleanup_action <- readIORef prelCleanupAfterRunAction
        case cleanup_action of
           Just xx -> xx
           Nothing -> return ()
        nh_stderr >>= nh_flush
        nh_stdout >>= nh_flush
        nh_stdin  >>= nh_close
        nh_exitwith (fromExitCode c)
        (ioError.IOError) "System.exitWith: should not return"

system :: String -> IO ExitCode
system cmd
   | null cmd
   = (ioError.IOError) "System.system: null command"
   | otherwise
   = do str    <- copy_String_to_cstring cmd
        status <- nh_system str
        nh_free str
        case status of
           0  -> return ExitSuccess
           n  -> return (ExitFailure n)

getPID :: IO Int
getPID
   = nh_getPID

-----------------------------------------------------------------------------
\end{code}
#endif
