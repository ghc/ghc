%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[Posix]{Haskell 1.3 POSIX bindings}

\begin{code}
module Posix  (
    module PosixDB,
    module PosixErr,
    module PosixFiles,
    module PosixIO,
    module PosixProcEnv,
    module PosixProcPrim,
    module PosixTTY,

    runProcess,

    ByteCount,
    Fd, intToFd,
    ClockTick,
    EpochTime,
    FileOffset,
    GroupID,
    Limit,
    LinkCount,
    ProcessID,
    ProcessGroupID,
    UserID,
    
    ExitCode

    )  where

import PrelBase
import PrelIOBase
import IO
import PrelHandle

import PosixDB
import PosixErr
import PosixFiles
import PosixIO
import PosixProcEnv
import PosixProcPrim
import PosixTTY
import PosixUtil

-- [OLD COMMENT:]
-- runProcess is our candidate for the high-level OS-independent primitive 
-- If accepted, it will be moved out of Posix into LibSystem.

import Directory	( setCurrentDirectory )


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
                  Just x  -> handleToFd x             >>= \ src ->
                             dupTo src (intToFd dest) >>
			     return ()

\end{code}
