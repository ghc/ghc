%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[PrimIO]{@PrimIO@ monad}

This sits on top of the state-transformer monad.  See
state-interface.verb.

We follow the Haskell~1.3 I/O proposal nomenclature.

\begin{code}
module PreludePrimIO (
	-- PrimIO(..): no, the compiler already knows about it

	fixPrimIO,
	listPrimIO,
	mapAndUnzipPrimIO,
	mapPrimIO,
	returnPrimIO,
	seqPrimIO,
	thenPrimIO,
	unsafePerformPrimIO,
	unsafeInterleavePrimIO,
--	forkPrimIO,

	-- all the Stdio stuff (this is how you get to it)
	-- (well, why not?)
	fclose, fdopen, fflush, fopen, fread, freopen,
	fwrite, _FILE(..),

	-- IVars and MVars come from here, too
	_IVar, _MVar,	-- abstract
	IVar(..), MVar(..), -- for convenience
	newEmptyMVar, takeMVar, putMVar, newMVar, readMVar, swapMVar,
	newIVar, readIVar, writeIVar,

	threadWait, threadDelay,

	-- backward compatibility -- don't use!
	readChanPrimIO,
	appendChanPrimIO,
	appendFilePrimIO,
	getArgsPrimIO,
	
	-- make interface self-sufficient
	fixST, unsafeInterleaveST
    ) where

import PreludeGlaST
import TyArray		( Array(..) )
import Cls
import Core
import List		( (++), map )
import PreludeDialogueIO ( processIORequest )
import PS		( _PackedString, _unpackPS )
import TyComplex
import TyIO
import Stdio

import PreludeMonadicIO	( IO(..), Either(..), return, (>>=), (>>) )
import PreludeIOError	( IOError13 )

infixr 1 `thenPrimIO`, `seqPrimIO`
\end{code}

%************************************************************************
%*									*
\subsection[IO-monad]{The @IO@ monad}
%*									*
%************************************************************************

\begin{code}
type PrimIO a     = _ST _RealWorld a
\end{code}

The usual business:
\begin{code}
{-# GENERATE_SPECS returnPrimIO a #-}
returnPrimIO    :: a -> PrimIO a

{-# GENERATE_SPECS thenPrimIO b #-}
thenPrimIO      :: PrimIO a -> (a -> PrimIO b) -> PrimIO b

{-# GENERATE_SPECS seqPrimIO b #-}
seqPrimIO	:: PrimIO a -> PrimIO b -> PrimIO b

fixPrimIO	:: (a -> PrimIO a) -> PrimIO a
listPrimIO	:: [PrimIO a] -> PrimIO [a]
mapPrimIO	:: (a -> PrimIO b) -> [a] -> PrimIO [b]
mapAndUnzipPrimIO :: (a -> PrimIO (b,c)) -> [a] -> PrimIO ([b],[c])

{-# INLINE returnPrimIO #-}
{-# INLINE thenPrimIO   #-}
{-# INLINE seqPrimIO  #-}

returnPrimIO x    s = returnStrictlyST x s
thenPrimIO   m k  s = thenStrictlyST  m k s
seqPrimIO    m k  s = seqStrictlyST   m k s

fixPrimIO   	    = fixST

listPrimIO []     = returnPrimIO []
listPrimIO (m:ms) = m			`thenPrimIO` \ x ->
		    listPrimIO ms	`thenPrimIO` \xs ->
		    returnPrimIO (x:xs)

-- An earlier definition of listPrimIO in terms of foldrPrimIO
-- was just wrong (it did the operations in the wrong order)
-- so I deleted foldrPrimIO and defined listPrimIO directly.
-- SLPJ Feb 95

mapPrimIO f ms = listPrimIO (map f ms)

mapAndUnzipPrimIO f []     = returnPrimIO ([], [])
mapAndUnzipPrimIO f (m:ms)
  = f m			    `thenPrimIO` \ ( r1,  r2) ->
    mapAndUnzipPrimIO f ms  `thenPrimIO` \ (rs1, rs2) ->
    returnPrimIO (r1:rs1, r2:rs2)
\end{code}

\begin{code}
{-# GENERATE_SPECS unsafePerformPrimIO a #-}
unsafePerformPrimIO    :: PrimIO a -> a

unsafeInterleavePrimIO :: PrimIO a -> PrimIO a

unsafePerformPrimIO k = case (k (S# realWorld#)) of (r, _) -> r

unsafeInterleavePrimIO m s = unsafeInterleaveST m s
\end{code}

Transitional: for pre-1.3 systems: Don't use them!
\begin{code}
readChanPrimIO	  :: String  ->		    PrimIO String
appendChanPrimIO  :: String  -> String	 -> PrimIO ()
appendFilePrimIO  :: String  -> String	 -> PrimIO ()
getArgsPrimIO	  ::			    PrimIO [String]

readChanPrimIO c	= processIORequestString  ( ReadChan c )
appendChanPrimIO c s	= processIORequestUnit	  ( AppendChan c s )
appendFilePrimIO f s	= processIORequestUnit	  ( AppendFile f s )
getArgsPrimIO		= processIORequestStrList ( GetArgs )

processIORequestUnit	:: Request -> PrimIO ()
processIORequestString	:: Request -> PrimIO String
processIORequestStrList :: Request -> PrimIO [String]

processIORequestUnit req
  = processIORequest req	`thenPrimIO` \ resp -> 
    case resp of
      Success	    -> returnPrimIO ()
      Failure ioerr -> error (ioErrMsg ioerr)
      _		    -> error "funny Response, expected a Success" 

processIORequestString req
  = processIORequest req	`thenPrimIO` \ resp -> 
    case resp of
      Str str	    -> returnPrimIO str
      Failure ioerr -> error (ioErrMsg ioerr)
      _		    -> error "funny Response, expected a String" 

processIORequestStrList req
  = processIORequest req	`thenPrimIO` \ resp -> 
    case resp of
      StrList strl  -> returnPrimIO strl
      Failure ioerr -> error (ioErrMsg ioerr)
      _		    -> error "funny Response, expected a [String]"

ioErrMsg :: IOError	      -> String
ioErrMsg    (ReadError s)   =	 "Read Error: " ++ s
ioErrMsg    (WriteError s)  =	 "Write Error: " ++ s
ioErrMsg    (FormatError s) =	 "Format Error: " ++ s
ioErrMsg    (SearchError s) =	 "Search Error: " ++ s
ioErrMsg    (OtherError s)  =	 "Other Error: " ++ s
\end{code}

%************************************************************************
%*									*
\subsection[PreludeGlaST-mvars]{M-Structures}
%*									*
%************************************************************************

M-Vars are rendezvous points for concurrent threads.  They begin
empty, and any attempt to read an empty M-Var blocks.  When an M-Var
is written, a single blocked thread may be freed.  Reading an M-Var
toggles its state from full back to empty.  Therefore, any value
written to an M-Var may only be read once.  Multiple reads and writes
are allowed, but there must be at least one read between any two
writes.

\begin{code}
data _MVar a = _MVar (SynchVar# _RealWorld a)
type MVar a = _MVar a
\end{code}

\begin{code}
newEmptyMVar  :: IO (_MVar a)

newEmptyMVar (S# s#) = 
    case newSynchVar# s# of
        StateAndSynchVar# s2# svar# -> (Right (_MVar svar#), S# s2#)

takeMVar :: _MVar a -> IO a

takeMVar (_MVar mvar#) (S# s#) =
    case takeMVar# mvar# s# of
        StateAndPtr# s2# r -> (Right r, S# s2#)

putMVar  :: _MVar a -> a -> IO ()

putMVar (_MVar mvar#) x (S# s#) =
    case putMVar# mvar# x s# of
        s2# -> (Right (), S# s2#)

newMVar :: a -> IO (_MVar a)

newMVar value =
    newEmptyMVar	>>= \ mvar ->
    putMVar mvar value	>>
    return mvar

readMVar :: _MVar a -> IO a

readMVar mvar =
    takeMVar mvar	>>= \ value ->
    putMVar mvar value	>>
    return value

swapMVar :: _MVar a -> a -> IO a

swapMVar mvar new =
    takeMVar mvar	>>= \ old ->
    putMVar mvar new	>>
    return old

\end{code}

%************************************************************************
%*									*
\subsection[PreludeGlaST-ivars]{I-Structures}
%*									*
%************************************************************************

I-Vars are write-once variables.  They start out empty, and any threads that 
attempt to read them will block until they are filled.  Once they are written, 
any blocked threads are freed, and additional reads are permitted.  Attempting 
to write a value to a full I-Var results in a runtime error.

\begin{code}
data _IVar a = _IVar (SynchVar# _RealWorld a)
type IVar a = _IVar a
\end{code}

\begin{code}
newIVar :: IO (_IVar a)

newIVar (S# s#) = 
    case newSynchVar# s# of
        StateAndSynchVar# s2# svar# -> (Right (_IVar svar#), S# s2#)

readIVar :: _IVar a -> IO a

readIVar (_IVar ivar#) (S# s#) =
    case readIVar# ivar# s# of
        StateAndPtr# s2# r -> (Right r, S# s2#)

writeIVar :: _IVar a -> a -> IO ()

writeIVar (_IVar ivar#) x (S# s#) =
    case writeIVar# ivar# x s# of
        s2# -> (Right (), S# s2#)

\end{code}


%************************************************************************
%*									*
\subsection{Thread Wait Functions}
%*									*
%************************************************************************

@threadDelay@ delays rescheduling of a thread until the indicated
number of microseconds have elapsed.  Generally, the microseconds are
counted by the context switch timer, which ticks in virtual time;
however, when there are no runnable threads, we don't accumulate any
virtual time, so we start ticking in real time.  (The granularity is
the effective resolution of the context switch timer, so it is
affected by the RTS -C option.)

@threadWait@ delays rescheduling of a thread until input on the
specified file descriptor is available for reading (just like select).

\begin{code}
threadDelay, threadWait :: Int -> IO ()

threadDelay (I# x#) (S# s#) = 
    case delay# x# s# of
      s2# -> (Right (), S# s2#)

threadWait (I# x#) (S# s#) = 
    case wait# x# s# of
      s2# -> (Right (), S# s2#)
\end{code}
