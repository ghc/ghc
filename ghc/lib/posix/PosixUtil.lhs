%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[PosixUtil]{Haskell 1.3 POSIX utilities}

\begin{code}
module PosixUtil where

import GlaExts
import PrelIOBase  -- IOError representation
\end{code}

First, all of the major Posix data types, to avoid any recursive dependencies

\begin{code}
type ByteCount		= Int
type ClockTick		= Int
type EpochTime		= Int
type FileOffset		= Int
type GroupID		= Int
type Limit		= Int
type LinkCount		= Int
type ProcessID		= Int
type ProcessGroupID	= ProcessID
type UserID		= Int
data Fd                 = FD# Int#
instance CCallable   Fd
instance CReturnable Fd

instance Eq Fd where
  (FD# x#) == (FD# y#) = x# ==# y#

-- use with care.
intToFd :: Int -> Fd
intToFd (I# fd#) = FD# fd#

fdToInt :: Fd -> Int
fdToInt (FD# x#) = I# x#
\end{code}

Now some local functions that shouldn't go outside this library.

Fail with a SystemError.  Normally, we do not try to re-interpret
POSIX error numbers, so most routines in this file will only fail
with SystemError.  The only exceptions are (1) those routines where
failure of some kind may be considered ``normal''...e.g. getpwnam()
for a non-existent user, or (2) those routines which do not set
errno.

\begin{code}
syserr :: String -> IO a
syserr str = fail (IOError Nothing     -- ToDo: better
			   SystemError
			   str
			   "")

-- common templates for system calls

nonzero_error :: IO Int -> String -> IO ()
nonzero_error io err = do
    rc <- io
    if rc == 0
       then return ()
       else syserr err

minusone_error :: IO Int -> String -> IO ()
minusone_error io err = do
    rc <- io
    if rc /= -1
       then return ()
       else syserr err

\end{code}
