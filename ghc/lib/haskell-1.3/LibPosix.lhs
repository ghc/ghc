%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibPosix]{Haskell 1.3 POSIX bindings}

\begin{code}
module LibPosix  (
    LibPosixDB..,
    LibPosixErr..,
    LibPosixFiles..,
    LibPosixIO..,
    LibPosixProcEnv..,
    LibPosixProcPrim..,
    LibPosixTTY..,

    runProcess,

    ByteCount(..),
    Channel(..),
    ClockTick(..),
    EpochTime(..),
    FileOffset(..),
    GroupID(..),
    Limit(..),
    LinkCount(..),
    ProcessID(..),
    ProcessGroupID(..),
    UserID(..),
    
    ExitCode,

    -- make interface complete:
    setCurrentDirectory{-pragmas-}, getCurrentDirectory{-pragmas-}

    )  where

import LibPosixDB
import LibPosixErr
import LibPosixFiles
import LibPosixIO
import LibPosixProcEnv
import LibPosixProcPrim
import LibPosixTTY
import LibPosixUtil

-- runProcess is our candidate for the high-level OS-independent primitive 
-- If accepted, it will be moved out of LibPosix into LibSystem.

import LibDirectory	( setCurrentDirectory, getCurrentDirectory{-pragmas-} )

import PreludeGlaST
import PreludePrimIO	( takeMVar, putMVar, _MVar )
import PreludeStdIO

runProcess :: FilePath			    -- Command
           -> [String]			    -- Arguments
           -> Maybe [(String, String)]	    -- Environment
           -> Maybe FilePath		    -- Working directory    
           -> Maybe Handle		    -- stdin
           -> Maybe Handle		    -- stdout
           -> Maybe Handle		    -- stderr
           -> IO ()
runProcess path args env dir stdin stdout stderr = 
    forkProcess >>= \ pid ->
    case pid of
      Nothing -> doTheBusiness
      Just x  -> return ()
  where
    doTheBusiness :: IO ()
    doTheBusiness =
        maybeChangeWorkingDirectory	>>
        maybeDup2 0 stdin		>>
        maybeDup2 1 stdout		>>
        maybeDup2 2 stderr		>>
        executeFile path True args env  >>
        syserr "runProcess"

    maybeChangeWorkingDirectory :: IO ()
    maybeChangeWorkingDirectory =
        case dir of
          Nothing -> return ()
          Just x  -> setCurrentDirectory x

    maybeDup2 :: Int -> Maybe Handle -> IO ()
    maybeDup2 dest h =
        case h of Nothing -> return ()
                  Just x  -> handleFD x >>= \ src ->
                             dupChannelTo src dest >>
			     return ()

    handleFD :: Handle -> IO Channel
    handleFD handle =
        takeMVar handle				    >>= \ htype ->
        putMVar handle htype		            >>
        case htype of 
          _ErrorHandle ioError -> failWith ioError
          _ClosedHandle -> failWith (IllegalOperation "handle is closed")
          _SemiClosedHandle _ _ -> failWith (IllegalOperation "handle is closed")
          other -> 
	    _casm_ ``%r = fileno((FILE *)%0);'' (_filePtr other)
						    `thenPrimIO` \ fd ->
	    return fd

\end{code}
