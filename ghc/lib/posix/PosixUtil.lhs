%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[PosixUtil]{Haskell 1.3 POSIX utilities}

\begin{code}
module PosixUtil where

import ST
import PrelST   -- ST representation
import PrelIOBase  -- IOError representation
import Addr
import Foreign
import CCall
import PrelBase
import MutableArray
import ByteArray
import Array
import PackedString	( packCBytesST, psToByteArrayST, unpackPS )
import Ix
import PrelArr          (StateAndMutableByteArray#(..), StateAndByteArray#(..))
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
			   str)

-- Allocate a mutable array of characters with no indices.

allocChars :: Int -> IO (MutableByteArray RealWorld ())
allocChars (I# size#) = IO $ \ s# ->
    case newCharArray# size# s# of
      StateAndMutableByteArray# s2# barr# ->
	IOok s2# (MutableByteArray bot barr#)
  where
    bot = error "PosixUtil.allocChars"

-- Allocate a mutable array of words with no indices

allocWords :: Int -> IO (MutableByteArray RealWorld ())
allocWords (I# size#) = IO $ \ s# ->
    case newIntArray# size# s# of
      StateAndMutableByteArray# s2# barr# ->
	IOok s2# (MutableByteArray bot barr#)
  where
    bot = error "PosixUtil.allocWords"

-- Freeze these index-free mutable arrays

freeze :: MutableByteArray RealWorld () -> IO (ByteArray ())
freeze (MutableByteArray ixs arr#) = IO $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of
      StateAndByteArray# s2# frozen# ->
	IOok s2# (ByteArray ixs frozen#)

-- Copy a null-terminated string from outside the heap to
-- Haskellized nonsense inside the heap

strcpy :: Addr -> IO String
strcpy str
  | str == ``NULL'' = return ""
  | otherwise =
    _ccall_ strlen str		    >>= \ len ->
    stToIO (packCBytesST len str)   >>= \ ps ->
    return (unpackPS ps)

-- Turn a string list into a NULL-terminated vector of null-terminated
-- strings No indices...I hate indices.  Death to Ix.

vectorize :: [String] -> IO (ByteArray ())
vectorize xs = do
  arr <- allocWords (len + 1)
  fill arr 0 xs
  freeze arr
 where
    len :: Int
    len = length xs

    fill :: MutableByteArray RealWorld () -> Int -> [String] -> IO ()
    fill arr n [] =
	_casm_ ``((PP_)%0)[%1] = NULL;'' arr n
    fill arr n (x:xs) =
	stToIO (psToByteArrayST x)	    >>= \ barr ->
        _casm_ ``((PP_)%0)[%1] = (P_)%2;'' arr n barr
					    >>= \ () ->
	fill arr (n+1) xs

-- Turn a NULL-terminated vector of null-terminated strings into a string list

unvectorize :: Addr -> Int -> IO [String]
unvectorize ptr n
  | str == ``NULL'' = return []
  | otherwise =
	strcpy str			    >>= \ x ->
	unvectorize ptr (n+1)		    >>= \ xs ->
	return (x : xs)
  where
    str = indexAddrOffAddr ptr n

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

-- IO versions of a few ST functions.

psToByteArrayIO = stToIO . psToByteArrayST

\end{code}
