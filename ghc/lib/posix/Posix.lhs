%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[Posix]{Haskell 1.3 POSIX bindings}

\begin{code}
{-# OPTIONS -#include "../std/cbits/stgio.h" #-}
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
--
-- ***NOTE***: make sure you completely force the evaluation of the path
-- and arguments to the child before calling runProcess. If you don't do
-- this *and* the arguments from runProcess are read in from a file lazily,
-- be prepared for some rather weird parent-child file I/O behaviour.
--
-- [If you don't force the args, consider the case where the
--  arguments emanate from a file that is read lazily, using hGetContents
--  or some such. Since a child of a fork() inherits the opened files of
--  the parent, the child can force the evaluation of the arguments and
--  read them off the file without any problems.  The problem is that
--  while the child share a file table with the parent, it has
--  separate buffers, so a child may fill up its (copy of) the buffer, but
--  only read it partially. When the *parent* tries to read from the shared file again,
--  the (shared) file offset will have been stepped on by whatever number of chars
--  that was copied into the file buffer of the child. i.e., the unused parts of the
--  buffer will *not* be seen, resulting in random/unpredicatable results.
--
--  Based on a true (, debugged :-) story.
-- ]

import Directory	( setCurrentDirectory )


runProcess :: FilePath			    -- Command
           -> [String]			    -- Arguments
           -> Maybe [(String, String)]	    -- Environment
           -> Maybe FilePath		    -- Working directory    
           -> Maybe Handle		    -- stdin
           -> Maybe Handle		    -- stdout
           -> Maybe Handle		    -- stderr
           -> IO ()
runProcess path args env dir stdin stdout stderr = do
    pid <- forkProcess
    case pid of
      Nothing -> doTheBusiness
      Just _  -> return ()
  where
    doTheBusiness :: IO ()
    doTheBusiness = do
        maybeChangeWorkingDirectory
        maybeDup2 0 stdin
        maybeDup2 1 stdout
        maybeDup2 2 stderr
        executeFile path True args env
        syserr "runProcess"

    maybeChangeWorkingDirectory :: IO ()
    maybeChangeWorkingDirectory =
        case dir of
          Nothing -> return ()
          Just x  -> setCurrentDirectory x

    maybeDup2 :: Int -> Maybe Handle -> IO ()
    maybeDup2 dest h =
        case h of Nothing -> return ()
                  Just x  -> do
		    src <- handleToFd x
                    dupTo src (intToFd dest)
		    return ()

\end{code}
