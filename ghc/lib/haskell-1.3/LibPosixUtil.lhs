%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibPosixUtil]{Haskell 1.3 POSIX utilities}

\begin{code}

module LibPosixUtil (
    LibPosixUtil..,

    _ByteArray,
    _MutableByteArray,
    _ST(..) 

    ) where

import PreludeGlaST
import PS

\end{code}

First, all of the major Posix data types, to avoid any recursive dependencies

\begin{code}

type ByteCount = Int
type Channel = Int
type ClockTick = Int
type EpochTime = Int
type FileOffset = Int
type GroupID = Int
type Limit = Int
type LinkCount = Int
type ProcessID = Int
type ProcessGroupID = ProcessID
type UserID = Int

\end{code}

Now some local fucntions that shouldn't go outside this library.

\begin{code}

-- Fail with a SystemError.  Normally, we do not try to re-interpret POSIX
-- error numbers, so most routines in this file will only fail with SystemError.
-- The only exceptions are (1) those routines where failure of some kind may be
-- considered ``normal''...e.g. getpwnam() for a non-existent user, or (2) those
-- routines which do not set errno.

syserr :: String -> IO a 
syserr = failWith . SystemError

-- Allocate a mutable array of characters with no indices.

allocChars :: Int -> _ST s (_MutableByteArray s ())
allocChars (I# size#) (S# s#) =
    case newCharArray# size# s# of 
      StateAndMutableByteArray# s2# barr# -> (_MutableByteArray bot barr#, S# s2#)
  where
    bot = error "allocChars{LibPosix}"

-- Allocate a mutable array of words with no indices

allocWords :: Int -> _ST s (_MutableByteArray s ())
allocWords (I# size#) (S# s#) =
    case newIntArray# size# s# of 
      StateAndMutableByteArray# s2# barr# -> (_MutableByteArray bot barr#, S# s2#)
  where
    bot = error "allocWords{LibPosix}"

-- Freeze these index-free mutable arrays

freeze :: _MutableByteArray s () -> _ST s (_ByteArray ())
freeze (_MutableByteArray ixs arr#) (S# s#) =
    case unsafeFreezeByteArray# arr# s# of
      StateAndByteArray# s2# frozen# -> (_ByteArray ixs frozen#, S# s2#)

-- Copy a null-terminated string from outside the heap to 
-- Haskellized nonsense inside the heap

strcpy :: _Addr -> PrimIO String
strcpy str
  | str == ``NULL'' = returnPrimIO ""
  | otherwise =
    _ccall_ strlen str			    `thenPrimIO` \ len ->
    _packCBytesST len str		    `thenStrictlyST` \ ps ->
    returnPrimIO (_unpackPS ps)

-- Turn a string list into a NULL-terminated vector of null-terminated strings
-- No indices...I hate indices.  Death to Ix.

vectorize :: [String] -> PrimIO (_ByteArray ())
vectorize xs =
    allocWords (len+1)				    `thenStrictlyST` \ arr ->
    fill arr 0 xs				    `thenPrimIO` \ () ->
    freeze arr					    `thenStrictlyST` \ frozen ->
    returnPrimIO frozen

  where
    len :: Int
    len = length xs

    fill :: _MutableByteArray _RealWorld () -> Int -> [String] -> PrimIO ()
    fill arr n [] = 
	_casm_ ``((PP_)%0)[%1] = NULL;'' arr n
    fill arr n (x:xs) =
        _packBytesForCST x			    `thenStrictlyST` \ barr ->
        _casm_ ``((PP_)%0)[%1] = (P_)%2;'' arr n barr
						    `thenPrimIO` \ () ->
	fill arr (n+1) xs

-- Turn a NULL-terminated vector of null-terminated strings into a string list

unvectorize :: _Addr -> Int -> PrimIO [String]
unvectorize ptr n 
  | str == ``NULL'' = returnPrimIO []
  | otherwise = 
	strcpy str				    `thenPrimIO` \ x ->
	unvectorize ptr (n+1)			    `thenPrimIO` \ xs ->
	returnPrimIO (x : xs)
  where str = indexAddrOffAddr ptr n

\end{code}
