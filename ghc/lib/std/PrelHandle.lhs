
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelHandle]{Module @PrelHandle@}

This module defines Haskell {\em handles} and the basic operations
which are supported for them.

\begin{code}
{-# OPTIONS -fno-implicit-prelude -#include "cbits/stgio.h" #-}
#include "cbits/error.h"

#ifndef __HUGS__ /* Hugs just includes this in PreludeBuiltin so no header needed */
module PrelHandle where

import PrelBase
import PrelAddr		( Addr, nullAddr )
import PrelArr		( newVar, readVar, writeVar, ByteArray(..) )
import PrelRead		( Read )
import PrelList 	( span )
import PrelIOBase
import PrelException
import PrelMaybe	( Maybe(..) )
import PrelEnum
import PrelNum
import PrelShow
import PrelAddr		( Addr, nullAddr )
import PrelNum		( toInteger, toBig )
import PrelPack         ( packString )
import PrelWeak		( addForeignFinalizer )
import Ix

#if __CONCURRENT_HASKELL__
import PrelConc
#endif

#ifndef __PARALLEL_HASKELL__
import PrelForeign  ( makeForeignObj )
#endif

#endif /* ndef(__HUGS__) */

#ifdef __HUGS__
#define cat2(x,y)  x/**/y
#define CCALL(fun) cat2(prim_,fun)
#define __CONCURRENT_HASKELL__
#define stToIO id
#define sizeof_int64 8
#else
#define CCALL(fun) _ccall_ fun
#define const_BUFSIZ ``BUFSIZ''
#define primPackString
#endif

#ifndef __PARALLEL_HASKELL__
#define FILE_OBJECT	    ForeignObj
#else
#define FILE_OBJECT	    Addr
#endif
\end{code}

%*********************************************************
%*							*
\subsection{Types @Handle@, @Handle__@}
%*							*
%*********************************************************

The @Handle@ and @Handle__@ types are defined in @IOBase@.

\begin{code}
{-# INLINE newHandle   #-}
{-# INLINE withHandle #-}
newHandle     :: Handle__ -> IO Handle

#if defined(__CONCURRENT_HASKELL__)

-- Use MVars for concurrent Haskell
newHandle hc  = newMVar	hc 	>>= \ h ->
	        return (Handle h)
#else 

-- Use ordinary MutableVars for non-concurrent Haskell
newHandle hc  = stToIO (newVar	hc 	>>= \ h ->
		        return (Handle h))
#endif
\end{code}

%*********************************************************
%*							*
\subsection{@withHandle@ operations}
%*							*
%*********************************************************

In the concurrent world, handles are locked during use.  This is done
by wrapping an MVar around the handle which acts as a mutex over
operations on the handle.

To avoid races, we use the following bracketing operations.  The idea
is to obtain the lock, do some operation and replace the lock again,
whether the operation succeeded or failed.  We also want to handle the
case where the thread receives an exception while processing the IO
operation: in these cases we also want to relinquish the lock.

There are three versions of @withHandle@: corresponding to the three
possible combinations of:

	- the operation may side-effect the handle
	- the operation may return a result

If the operation generates an error or an exception is raised, the
orignal handle is always replaced [ this is the case at the moment,
but we might want to revisit this in the future --SDM ].

\begin{code}
#ifdef __CONCURRENT_HASKELL__
withHandle :: Handle -> (Handle__ -> IO (Handle__,a)) -> IO a
withHandle (Handle h) act = do
   h_ <- takeMVar h
   (h',v)  <- catchException (act h_) (\ ex -> putMVar h h_ >> throw ex)
   putMVar h h'
   return v

withHandle_ :: Handle -> (Handle__ -> IO a) -> IO a
withHandle_ (Handle h) act = do
   h_ <- takeMVar h
   v  <- catchException (act h_) (\ ex -> putMVar h h_ >> throw ex)
   putMVar h h_
   return v
   
withHandle__ :: Handle -> (Handle__ -> IO Handle__) -> IO ()
withHandle__ (Handle h) act = do
   h_ <- takeMVar h
   h'  <- catchException (act h_) (\ ex -> putMVar h h_ >> throw ex)
   putMVar h h'
   return ()

#else
   -- of questionable value to install this exception
   -- handler, but let's do it in the non-concurrent
   -- case too, for now.
withHandle (Handle h) act = do
   h_ <- stToIO (readVar h)
   v  <- catchException (act h_) (\ ex -> stToIO (writeVar h h_) >> throw ex)
   return v

#endif
\end{code}

nullFile__ is only used for closed handles, plugging it in as a null
file object reference.

\begin{code}
nullFile__ :: FILE_OBJECT
nullFile__ = 
#ifndef __PARALLEL_HASKELL__
    unsafePerformIO (makeForeignObj nullAddr)
#else
    nullAddr
#endif


mkClosedHandle__ :: Handle__
mkClosedHandle__ = 
  Handle__ 
	   nullFile__
	   ClosedHandle 
	   NoBuffering
	   "closed file"

mkErrorHandle__ :: IOError -> Handle__
mkErrorHandle__ ioe =
  Handle__
           nullFile__ 
	   (ErrorHandle ioe)
	   NoBuffering
	   "error handle"
\end{code}

%*********************************************************
%*							*
\subsection{Handle Finalizers}
%*							*
%*********************************************************

\begin{code}
#ifndef __HUGS__
freeStdFileObject :: ForeignObj -> IO ()
freeStdFileObject fo = CCALL(freeStdFileObject) fo

freeFileObject :: ForeignObj -> IO ()
freeFileObject fo = CCALL(freeFileObject) fo
#else
foreign import stdcall "libHS_cbits.so" "freeStdFileObject" freeStdFileObject :: ForeignObj -> IO ()
foreign import stdcall "libHS_cbits.so" "freeFileObject" freeFileObject :: ForeignObj -> IO ()
#endif
\end{code}

%*********************************************************
%*							*
\subsection[StdHandles]{Standard handles}
%*							*
%*********************************************************

Three handles are allocated during program initialisation.  The first
two manage input or output from the Haskell program's standard input
or output channel respectively.  The third manages output to the
standard error channel. These handles are initially open.


\begin{code}
stdin, stdout, stderr :: Handle

stdout = unsafePerformIO (do
    rc <- CCALL(getLock) (1::Int) (1::Int)   -- ConcHask: SAFE, won't block
    case (rc::Int) of
       0 -> newHandle (mkClosedHandle__)
       1 -> do
#ifndef __CONCURRENT_HASKELL__
 	    fo <- CCALL(openStdFile) (1::Int) 
				     (1::Int){-flush on close-}
				     (0::Int){-writeable-}  -- ConcHask: SAFE, won't block
#else
 	    fo <- CCALL(openStdFile) (1::Int)
				     ((1{-flush on close-} + 128 {- don't block on I/O-})::Int)
				     (0::Int){-writeable-}  -- ConcHask: SAFE, won't block
#endif

#ifndef __PARALLEL_HASKELL__
            fo <- makeForeignObj fo
	    addForeignFinalizer fo (freeStdFileObject fo)
#endif

#ifdef __HUGS__
/* I dont care what the Haskell report says, in an interactive system,
 * stdout should be unbuffered by default.
 */
            let bm = NoBuffering
#else
	    (bm, bf_size)  <- getBMode__ fo
	    mkBuffer__ fo bf_size
#endif
	    newHandle (Handle__ fo WriteHandle bm "stdout")
       _ -> do ioError <- constructError "stdout"
               newHandle (mkErrorHandle__ ioError)
  )

stdin = unsafePerformIO (do
    rc <- CCALL(getLock) (0::Int) (0::Int)   -- ConcHask: SAFE, won't block
    case (rc::Int) of
       0 -> newHandle (mkClosedHandle__)
       1 -> do
#ifndef __CONCURRENT_HASKELL__
	    fo <- CCALL(openStdFile) (0::Int)
				     (0::Int){-don't flush on close -}
				     (1::Int){-readable-}  -- ConcHask: SAFE, won't block
#else
	    fo <- CCALL(openStdFile) (0::Int)
				     ((0{-flush on close-} + 128 {- don't block on I/O-})::Int)
				     (1::Int){-readable-}  -- ConcHask: SAFE, won't block
#endif

#ifndef __PARALLEL_HASKELL__
            fo <- makeForeignObj fo
	    addForeignFinalizer fo (freeStdFileObject fo)
#endif
	    (bm, bf_size) <- getBMode__ fo
	    mkBuffer__ fo bf_size
	    hdl <- newHandle (Handle__ fo ReadHandle bm "stdin")
	     -- when stdin and stdout are both connected to a terminal, ensure
	     -- that anything buffered on stdout is flushed prior to reading from stdin.
	     -- 
	    hConnectTerms stdout hdl
	    return hdl
       _ -> do ioError <- constructError "stdin"
               newHandle (mkErrorHandle__ ioError)
  )


stderr = unsafePerformIO (do
    rc <- CCALL(getLock) (2::Int) (1::Int){-writeable-}  -- ConcHask: SAFE, won't block
    case (rc::Int) of
       0 -> newHandle (mkClosedHandle__)
       1 -> do
#ifndef __CONCURRENT_HASKELL__
 	    fo <- CCALL(openStdFile) (2::Int)
				     (1::Int){-flush on close-}
				     (0::Int){-writeable-} -- ConcHask: SAFE, won't block
#else
 	    fo <- CCALL(openStdFile) (2::Int)
				     ((1{-flush on close-} + 128 {- don't block on I/O-})::Int)
				     (0::Int){-writeable-} -- ConcHask: SAFE, won't block
#endif

#ifndef __PARALLEL_HASKELL__
            fo <- makeForeignObj fo
	    addForeignFinalizer fo (freeStdFileObject fo)
#endif
            hdl <- newHandle (Handle__ fo WriteHandle NoBuffering "stderr")
	    -- when stderr and stdout are both connected to a terminal, ensure
	    -- that anything buffered on stdout is flushed prior to writing to
	    -- stderr.
	    hConnectTo stdout hdl
	    return hdl

       _ -> do ioError <- constructError "stderr"
               newHandle (mkErrorHandle__ ioError)
  )
\end{code}

%*********************************************************
%*							*
\subsection[OpeningClosing]{Opening and Closing Files}
%*							*
%*********************************************************

\begin{code}
data IOMode      =  ReadMode | WriteMode | AppendMode | ReadWriteMode
                    deriving (Eq, Ord, Ix, Enum, Read, Show)

data IOModeEx 
 = BinaryMode IOMode
 | TextMode   IOMode
   deriving (Eq, Read, Show)

openFile :: FilePath -> IOMode -> IO Handle
openFile fp im = openFileEx fp (TextMode im)

openFileEx :: FilePath -> IOModeEx -> IO Handle

openFileEx f m = do
    fo <- CCALL(openFile) (primPackString f) (file_mode::Int) 
					     (binary::Int)
					     (file_flags::Int) -- ConcHask: SAFE, won't block
    if fo /= nullAddr then do
#ifndef __PARALLEL_HASKELL__
	fo  <- makeForeignObj fo
	addForeignFinalizer fo (freeFileObject fo)
#endif
	(bm, bf_size)  <- getBMode__ fo
        mkBuffer__ fo bf_size
	newHandle (Handle__ fo htype bm f)
      else do
	constructErrorAndFailWithInfo "openFile" f
  where
    (imo, binary) =
      case m of
        BinaryMode bmo -> (bmo, 1)
	TextMode tmo   -> (tmo, 0)

#ifndef __CONCURRENT_HASKELL__
    file_flags = file_flags'
#else
	-- See comment next to 'stderr' for why we leave
	-- non-blocking off for now.
    file_flags = file_flags' + 128  -- Don't block on I/O
#endif

    (file_flags', file_mode) =
      case imo of
           AppendMode    -> (1, 0)
           WriteMode     -> (1, 1)
           ReadMode      -> (0, 2)
           ReadWriteMode -> (1, 3)

    htype = case imo of 
              ReadMode      -> ReadHandle
              WriteMode     -> WriteHandle
              AppendMode    -> AppendHandle
              ReadWriteMode -> ReadWriteHandle
\end{code}

Computation $openFile file mode$ allocates and returns a new, open
handle to manage the file {\em file}.  It manages input if {\em mode}
is $ReadMode$, output if {\em mode} is $WriteMode$ or $AppendMode$,
and both input and output if mode is $ReadWriteMode$.

If the file does not exist and it is opened for output, it should be
created as a new file.  If {\em mode} is $WriteMode$ and the file
already exists, then it should be truncated to zero length.  The
handle is positioned at the end of the file if {\em mode} is
$AppendMode$, and otherwise at the beginning (in which case its
internal position is 0).

Implementations should enforce, locally to the Haskell process,
multiple-reader single-writer locking on files, which is to say that
there may either be many handles on the same file which manage input,
or just one handle on the file which manages output.  If any open or
semi-closed handle is managing a file for output, no new handle can be
allocated for that file.  If any open or semi-closed handle is
managing a file for input, new handles can only be allocated if they
do not manage output.

Two files are the same if they have the same absolute name.  An
implementation is free to impose stricter conditions.

\begin{code}
hClose :: Handle -> IO ()

hClose handle =
    withHandle__ handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError -> ioError theError
      ClosedHandle 	   -> return handle_
      _ -> do
          rc      <- CCALL(closeFile) (haFO__ handle_) (1::Int){-flush if you can-}  -- ConcHask: SAFE, won't block
          {- We explicitly close a file object so that we can be told
             if there were any errors. Note that after @hClose@
             has been performed, the ForeignObj embedded in the Handle
             is still lying around in the heap, so care is taken
             to avoid closing the file object when the ForeignObj
             is finalized. (we overwrite the file ptr in the underlying
	     FileObject with a NULL as part of closeFile())
	  -}
          if rc == (0::Int)
	   then return (handle_{ haType__   = ClosedHandle,
			         haFO__     = nullFile__ })
           else constructErrorAndFail "hClose"

\end{code}

Computation $hClose hdl$ makes handle {\em hdl} closed.  Before the
computation finishes, any items buffered for output and not already
sent to the operating system are flushed as for $flush$.

%*********************************************************
%*							*
\subsection[EOF]{Detecting the End of Input}
%*							*
%*********************************************************


For a handle {\em hdl} which attached to a physical file, $hFileSize
hdl$ returns the size of {\em hdl} in terms of the number of items
which can be read from {\em hdl}.

\begin{code}
hFileSize :: Handle -> IO Integer
hFileSize handle =
    withHandle_ handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError 	-> ioError theError
      ClosedHandle 		-> ioe_closedHandle "hFileSize" handle
      SemiClosedHandle 		-> ioe_closedHandle "hFileSize" handle
#ifdef __HUGS__
      _ -> do
          mem <- primNewByteArray sizeof_int64
          rc <- CCALL(fileSize_int64) (haFO__ handle_) mem  -- ConcHask: SAFE, won't block
          if rc == 0 then do
	     result <- primReadInt64Array mem 0
             return (primInt64ToInteger result)
           else 
             constructErrorAndFail "hFileSize"
#else
      _ ->
          -- HACK!  We build a unique MP_INT of the right shape to hold
          -- a single unsigned word, and we let the C routine 
	  -- change the data bits
	  --
	  -- For some reason, this fails to typecheck if converted to a do
	  -- expression --SDM
          _casm_ ``%r = 1;'' >>= \(I# hack#) ->
          case int2Integer# hack# of
              (# s, d #) -> do
                rc <- CCALL(fileSize) (haFO__ handle_) d  -- ConcHask: SAFE, won't block
                if rc == (0::Int) then
		   return (J# s d)
                 else
		   constructErrorAndFail "hFileSize"
#endif
\end{code}

For a readable handle {\em hdl}, @hIsEOF hdl@ returns
@True@ if no further input can be taken from @hdl@ or for a
physical file, if the current I/O position is equal to the length of
the file.  Otherwise, it returns @False@.

\begin{code}
hIsEOF :: Handle -> IO Bool
hIsEOF handle =
    wantReadableHandle "hIsEOF" handle $ \ handle_ -> do
    let fo = haFO__ handle_
    rc      <- mayBlock fo (CCALL(fileEOF) fo)  -- ConcHask: UNSAFE, may block
    case rc of
      0 -> return False
      1 -> return True
      _ -> constructErrorAndFail "hIsEOF"

isEOF :: IO Bool
isEOF = hIsEOF stdin
\end{code}

%*********************************************************
%*							*
\subsection[Buffering]{Buffering Operations}
%*							*
%*********************************************************

Three kinds of buffering are supported: line-buffering, 
block-buffering or no-buffering.  See @IOBase@ for definition
and further explanation of what the type represent.

Computation @hSetBuffering hdl mode@ sets the mode of buffering for
handle {\em hdl} on subsequent reads and writes.

\begin{itemize}
\item
If {\em mode} is @LineBuffering@, line-buffering should be
enabled if possible.
\item
If {\em mode} is @BlockBuffering@ {\em size}, then block-buffering
should be enabled if possible.  The size of the buffer is {\em n} items
if {\em size} is @Just@~{\em n} and is otherwise implementation-dependent.
\item
If {\em mode} is @NoBuffering@, then buffering is disabled if possible.
\end{itemize}

If the buffer mode is changed from @BlockBuffering@ or @LineBuffering@
to @NoBuffering@, then any items in the output buffer are written to
the device, and any items in the input buffer are discarded.  The
default buffering mode when a handle is opened is
implementation-dependent and may depend on the object which is
attached to that handle.

\begin{code}
hSetBuffering :: Handle -> BufferMode -> IO ()

hSetBuffering handle mode =
    case mode of
      BlockBuffering (Just n) 
        | n <= 0 -> ioError
		         (IOError (Just handle)
				  InvalidArgument
			          "hSetBuffering"
				  ("illegal buffer size " ++ showsPrec 9 n []))  -- 9 => should be parens'ified.
      _ ->
          withHandle__ handle $ \ handle_ -> do
          case haType__ handle_ of
	     ErrorHandle theError -> ioError theError
             ClosedHandle 	  -> ioe_closedHandle "hSetBuffering" handle
             _ -> do
	        {- Note:
		    - we flush the old buffer regardless of whether
		      the new buffer could fit the contents of the old buffer 
		      or not.
		    - allow a handle's buffering to change even if IO has
		      occurred (ANSI C spec. does not allow this, nor did
		      the previous implementation of IO.hSetBuffering).
		    - a non-standard extension is to allow the buffering
		      of semi-closed handles to change [sof 6/98]
		-}
		let fo = haFO__ handle_
                rc <- mayBlock fo (CCALL(setBuffering) fo bsize) -- ConcHask: UNSAFE, may block
                if rc == 0 
		 then do
		   return (handle_{ haBufferMode__ = mode })
                 else do
		   -- Note: failure to change the buffer size will cause old buffer to be flushed.
		   constructErrorAndFail "hSetBuffering"
  where
    bsize :: Int
    bsize = case mode of
              NoBuffering	      ->  0
              LineBuffering	      -> -1
              BlockBuffering Nothing  -> -2
              BlockBuffering (Just n) ->  n
\end{code}

The action @hFlush hdl@ causes any items buffered for output
in handle {\em hdl} to be sent immediately to the operating
system.

\begin{code}
hFlush :: Handle -> IO () 
hFlush handle =
    wantWriteableHandle "hFlush" handle $ \ handle_ -> do
    let fo = haFO__ handle_
    rc	    <- mayBlock fo (CCALL(flushFile) fo)   -- ConcHask: UNSAFE, may block
    if rc == 0 then 
       return ()
     else
       constructErrorAndFail "hFlush"

\end{code}


%*********************************************************
%*							*
\subsection[Seeking]{Repositioning Handles}
%*							*
%*********************************************************

\begin{code}
data HandlePosn
 = HandlePosn 
	Handle   -- Q: should this be a weak or strong ref. to the handle?
	Int

data SeekMode    =  AbsoluteSeek | RelativeSeek | SeekFromEnd
                    deriving (Eq, Ord, Ix, Enum, Read, Show)
\end{code}

Computation @hGetPosn hdl@ returns the current I/O
position of {\em hdl} as an abstract position.  Computation
$hSetPosn p$ sets the position of {\em hdl}
to a previously obtained position {\em p}.

\begin{code}
hGetPosn :: Handle -> IO HandlePosn
hGetPosn handle =
    wantSeekableHandle "hGetPosn" handle $ \ handle_ -> do
    posn    <- CCALL(getFilePosn) (haFO__ handle_)   -- ConcHask: SAFE, won't block
    if posn /= -1 then do
      return (HandlePosn handle posn)
     else
      constructErrorAndFail "hGetPosn"

hSetPosn :: HandlePosn -> IO () 
hSetPosn (HandlePosn handle posn) = 
    wantSeekableHandle "hSetPosn" handle $ \ handle_ -> do -- not as silly as it looks: the handle may have been closed in the meantime.
    let fo = haFO__ handle_
    rc     <- mayBlock fo (CCALL(setFilePosn) fo posn)    -- ConcHask: UNSAFE, may block
    if rc == 0 then do
       return ()
     else
	constructErrorAndFail "hSetPosn"
\end{code}

The action @hSeek hdl mode i@ sets the position of handle
@hdl@ depending on @mode@.  If @mode@ is
\begin{itemize}
\item[{\bf AbsoluteSeek}] The position of @hdl@ is set to @i@.
\item[{\bf RelativeSeek}] The position of @hdl@ is set to offset @i@ from
the current position.
\item[{\bf SeekFromEnd}] The position of @hdl@ is set to offset @i@ from
the end of the file.
\end{itemize}

Some handles may not be seekable (see @hIsSeekable@), or only support a
subset of the possible positioning operations (e.g. it may only be
possible to seek to the end of a tape, or to a positive offset from
the beginning or current position).

It is not possible to set a negative I/O position, or for a physical
file, an I/O position beyond the current end-of-file. 

Note: 
 - when seeking using @SeekFromEnd@, positive offsets (>=0) means seeking
   at or past EOF.
 - relative seeking on buffered handles can lead to non-obvious results.

\begin{code}
hSeek :: Handle -> SeekMode -> Integer -> IO () 
#ifdef __HUGS__
hSeek handle mode offset = 
    wantSeekableHandle "hSeek" handle $ \ handle_ -> do
    let fo = haFO__ handle_
    rc      <- mayBlock fo (CCALL(seekFile_int64) fo whence (primIntegerToInt64 offset))  -- ConcHask: UNSAFE, may block
#else
hSeek handle mode i@(S# _) = hSeek handle mode (toBig i)
hSeek handle mode (J# s# d#) =
    wantSeekableHandle "hSeek" handle $ \ handle_ -> do
    let fo = haFO__ handle_
    rc      <- mayBlock fo (CCALL(seekFile) fo whence (I# s#) d#)  -- ConcHask: UNSAFE, may block
#endif
    if rc == 0 then do
       return ()
     else
	constructErrorAndFail "hSeek"
  where
    whence :: Int
    whence = case mode of
               AbsoluteSeek -> 0
               RelativeSeek -> 1
               SeekFromEnd  -> 2
\end{code}

%*********************************************************
%*							*
\subsection[Query]{Handle Properties}
%*							*
%*********************************************************

A number of operations return information about the properties of a
handle.  Each of these operations returns $True$ if the
handle has the specified property, and $False$
otherwise.

Computation $hIsBlockBuffered hdl$ returns $( False, Nothing )$ if
{\em hdl} is not block-buffered.  Otherwise it returns 
$( True, size )$, where {\em size} is $Nothing$ for default buffering, and 
$( Just n )$ for block-buffering of {\em n} bytes.

\begin{code}
hIsOpen :: Handle -> IO Bool
hIsOpen handle =
    withHandle_ handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError -> ioError theError
      ClosedHandle         -> return False
      SemiClosedHandle     -> return False
      _ 		   -> return True

hIsClosed :: Handle -> IO Bool
hIsClosed handle =
    withHandle_ handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError -> ioError theError
      ClosedHandle 	   -> return True
      _ 		   -> return False

{- not defined, nor exported, but mentioned
   here for documentation purposes:

    hSemiClosed :: Handle -> IO Bool
    hSemiClosed h = do
       ho <- hIsOpen h
       hc <- hIsClosed h
       return (not (ho || hc))
-}

hIsReadable :: Handle -> IO Bool
hIsReadable handle =
    withHandle_ handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError -> ioError theError
      ClosedHandle 	   -> ioe_closedHandle "hIsReadable" handle
      SemiClosedHandle 	   -> ioe_closedHandle "hIsReadable" handle
      htype 		   -> return (isReadable htype)
  where
    isReadable ReadHandle      = True
    isReadable ReadWriteHandle = True
    isReadable _	       = False

hIsWritable :: Handle -> IO Bool
hIsWritable handle =
    withHandle_ handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError -> ioError theError
      ClosedHandle 	   -> ioe_closedHandle "hIsWritable" handle
      SemiClosedHandle 	   -> ioe_closedHandle "hIsWritable" handle
      htype 		   -> return (isWritable htype)
  where
    isWritable AppendHandle    = True
    isWritable WriteHandle     = True
    isWritable ReadWriteHandle = True
    isWritable _	       = False


#ifndef __PARALLEL_HASKELL__
getBMode__ :: ForeignObj -> IO (BufferMode, Int)
#else
getBMode__ :: Addr -> IO (BufferMode, Int)
#endif
getBMode__ fo = do
  rc <- CCALL(getBufferMode) fo    -- ConcHask: SAFE, won't block
  case (rc::Int) of
    0  -> return (NoBuffering, 0)
    -1 -> return (LineBuffering, default_buffer_size)
    -2 -> return (BlockBuffering Nothing, default_buffer_size)
    -3 -> return (NoBuffering, 0)		-- only happens on un-stat()able files.
    n  -> return (BlockBuffering (Just n), n)
 where
   default_buffer_size :: Int
   default_buffer_size = (const_BUFSIZ - 1)
\end{code}

Querying how a handle buffers its data:

\begin{code}
hGetBuffering :: Handle -> IO BufferMode
hGetBuffering handle = 
    withHandle_ handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError -> ioError theError
      ClosedHandle 	   -> ioe_closedHandle "hGetBuffering" handle
      _ -> 
	  {-
	   We're being non-standard here, and allow the buffering
	   of a semi-closed handle to be queried.   -- sof 6/98
          -}
	  return (haBufferMode__ handle_)  -- could be stricter..
\end{code}

\begin{code}
hIsSeekable :: Handle -> IO Bool
hIsSeekable handle =
    withHandle_ handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError -> ioError theError
      ClosedHandle 	   -> ioe_closedHandle "hIsSeekable" handle
      SemiClosedHandle 	   -> ioe_closedHandle "hIsSeekable" handle
      AppendHandle 	   -> return False
      _ -> do
	  rc <- CCALL(seekFileP) (haFO__ handle_)   -- ConcHask: SAFE, won't block
	  case (rc::Int) of
            0 -> return False
            1 -> return True
            _ -> constructErrorAndFail "hIsSeekable"
\end{code}


%*********************************************************
%*							*
\subsection{Changing echo status}
%*							*
%*********************************************************

Non-standard GHC extension is to allow the echoing status
of a handles connected to terminals to be reconfigured:

\begin{code}
hSetEcho :: Handle -> Bool -> IO ()
hSetEcho handle on = do
    isT   <- hIsTerminalDevice handle
    if not isT
     then return ()
     else
      withHandle_ handle $ \ handle_ -> do
      case haType__ handle_ of 
         ErrorHandle theError -> ioError theError
         ClosedHandle	      -> ioe_closedHandle "hSetEcho" handle
         _ -> do
            rc <- CCALL(setTerminalEcho) (haFO__ handle_) ((if on then 1 else 0)::Int)  -- ConcHask: SAFE, won't block
	    if rc /= ((-1)::Int)
	     then return ()
	     else constructErrorAndFail "hSetEcho"

hGetEcho :: Handle -> IO Bool
hGetEcho handle = do
    isT   <- hIsTerminalDevice handle
    if not isT
     then return False
     else
       withHandle_ handle $ \ handle_ -> do
       case haType__ handle_ of 
         ErrorHandle theError -> ioError theError
         ClosedHandle	      -> ioe_closedHandle "hGetEcho" handle
         _ -> do
            rc <- CCALL(getTerminalEcho) (haFO__ handle_)  -- ConcHask: SAFE, won't block
	    case (rc::Int) of
	      1 -> return True
	      0 -> return False
	      _ -> constructErrorAndFail "hSetEcho"

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice handle = do
    withHandle_ handle $ \ handle_ -> do
     case haType__ handle_ of 
       ErrorHandle theError -> ioError theError
       ClosedHandle	    -> ioe_closedHandle "hIsTerminalDevice" handle
       _ -> do
          rc <- CCALL(isTerminalDevice) (haFO__ handle_)   -- ConcHask: SAFE, won't block
	  case (rc::Int) of
	    1 -> return True
	    0 -> return False
	    _ -> constructErrorAndFail "hIsTerminalDevice"
\end{code}

\begin{code}
hConnectTerms :: Handle -> Handle -> IO ()
hConnectTerms hW hR = hConnectHdl_ hW hR 1{-check if they're both coming connected to ttys-}

hConnectTo :: Handle -> Handle -> IO ()
hConnectTo hW hR = hConnectHdl_ hW hR 0{-connect regardless-}

hConnectHdl_ :: Handle -> Handle -> Int -> IO ()
hConnectHdl_ hW hR is_tty =
  wantRWHandle "hConnectTo" hW $ \ hW_ ->
  wantRWHandle "hConnectTo" hR $ \ hR_ -> do
  CCALL(setConnectedTo) (haFO__ hR_) (haFO__ hW_) is_tty  -- ConcHask: SAFE, won't block

#ifndef __PARALLEL_HASKELL__
#define FILE_OBJECT     ForeignObj
#else
#define FILE_OBJECT     Addr
#endif

flushConnectedBuf :: FILE_OBJECT -> IO ()
flushConnectedBuf fo = CCALL(flushConnectedBuf) fo
\end{code}

As an extension, we also allow characters to be pushed back.
Like ANSI C stdio, we guarantee no more than one character of
pushback. (For unbuffered channels, the (default) push-back limit is
2 chars tho.)

\begin{code}
hUngetChar :: Handle -> Char -> IO ()
hUngetChar handle c = 
    wantReadableHandle "hLookAhead" handle $ \ handle_ -> do
    rc      <- CCALL(ungetChar) (haFO__ handle_) c  -- ConcHask: SAFE, won't block
    if rc == ((-1)::Int)
     then constructErrorAndFail "hUngetChar"
     else return ()

\end{code}


Hoisting files in in one go is sometimes useful, so we support
this as an extension:

\begin{code}
-- in one go, read file into an externally allocated buffer.
slurpFile :: FilePath -> IO (Addr, Int)
slurpFile fname = do
  handle <- openFile fname ReadMode
  sz     <- hFileSize handle
  if sz > toInteger (maxBound::Int) then 
    ioError (userError "slurpFile: file too big")
   else do
     let sz_i = fromInteger sz
     chunk <- CCALL(allocMemory__) (sz_i::Int)
     if chunk == nullAddr 
      then do
        hClose handle
        constructErrorAndFail "slurpFile"
      else do
        rc <- withHandle_ handle ( \ handle_ -> do
          let fo = haFO__ handle_
	  mayBlock fo (CCALL(readChunk) fo chunk sz_i)    -- ConcHask: UNSAFE, may block.
	 )
	hClose handle
        if rc < (0::Int)
	 then constructErrorAndFail "slurpFile"
	 else return (chunk, rc)

#ifndef __HUGS__ /* Hugs' Prelude doesn't need this */
hFillBufBA :: Handle -> ByteArray Int -> Int -> IO Int
hFillBufBA handle buf sz
  | sz <= 0 = ioError (IOError (Just handle)
			    InvalidArgument
		            "hFillBufBA"
			    ("illegal buffer size " ++ showsPrec 9 sz []))  -- 9 => should be parens'ified.
  | otherwise = 
    wantReadableHandle "hFillBufBA" handle $ \ handle_ -> do
    let fo  = haFO__ handle_
#ifdef __HUGS__
    rc      <- mayBlock fo (CCALL(readChunkBA) fo buf sz)    -- ConcHask: UNSAFE, may block.
#else
    rc      <- mayBlock fo (CCALL(readChunk) fo buf sz)    -- ConcHask: UNSAFE, may block.
#endif
    if rc >= (0::Int)
     then return rc
     else constructErrorAndFail "hFillBufBA"
#endif

hFillBuf :: Handle -> Addr -> Int -> IO Int
hFillBuf handle buf sz
  | sz <= 0 = ioError (IOError (Just handle)
			    InvalidArgument
		            "hFillBuf"
			    ("illegal buffer size " ++ showsPrec 9 sz []))  -- 9 => should be parens'ified.
  | otherwise = 
    wantReadableHandle "hFillBuf" handle $ \ handle_ -> do
    let fo  = haFO__ handle_
    rc      <- mayBlock fo (CCALL(readChunk) fo buf sz)    -- ConcHask: UNSAFE, may block.
    if rc >= 0
     then return rc
     else constructErrorAndFail "hFillBuf"

\end{code}

The @hPutBuf hdl buf len@ action writes an already packed sequence of
bytes to the file/channel managed by @hdl@ - non-standard.

\begin{code}
hPutBuf :: Handle -> Addr -> Int -> IO ()
hPutBuf handle buf len = 
    wantWriteableHandle "hPutBuf" handle $ \ handle_ -> do
    let fo  = haFO__ handle_
    rc      <- mayBlock fo (CCALL(writeBuf) fo buf len)  -- ConcHask: UNSAFE, may block.
    if rc == (0::Int)
     then return ()
     else constructErrorAndFail "hPutBuf"

#ifndef __HUGS__ /* An_ one Hugs doesn't provide */
hPutBufBA :: Handle -> ByteArray Int -> Int -> IO ()
hPutBufBA handle buf len =
    wantWriteableHandle "hPutBufBA" handle $ \ handle_ -> do
    let fo = haFO__ handle_
    rc      <- mayBlock fo (CCALL(writeBufBA) fo buf len)  -- ConcHask: UNSAFE, may block.
    if rc == (0::Int)
     then return ()
     else constructErrorAndFail "hPutBuf"
#endif
\end{code}

Sometimes it's useful to get at the file descriptor that
the Handle contains..

\begin{code}
getHandleFd :: Handle -> IO Int
getHandleFd handle =
    withHandle_ handle $ \ handle_ -> do
    case (haType__ handle_) of
      ErrorHandle theError -> ioError theError
      ClosedHandle 	   -> ioe_closedHandle "getHandleFd" handle
      _ -> do
          fd <- CCALL(getFileFd) (haFO__ handle_)
	  return fd
\end{code}


%*********************************************************
%*							*
\subsection{Miscellaneous}
%*							*
%*********************************************************

These three functions are meant to get things out of @IOErrors@.

(ToDo: improve!)

\begin{code}
ioeGetFileName        :: IOError -> Maybe FilePath
ioeGetErrorString     :: IOError -> String
ioeGetHandle          :: IOError -> Maybe Handle

ioeGetHandle   (IOError h _ _ _)   = h
ioeGetErrorString (IOError _ iot _ str) =
 case iot of
   EOF -> "end of file"
   _   -> str

ioeGetFileName (IOError _ _  _ str) = 
 case span (/=':') str of
   (_,[])  -> Nothing
   (fs,_)  -> Just fs

\end{code}

'Top-level' IO actions want to catch exceptions (e.g., forkIO and 
PrelMain.mainIO) and report them - topHandler is the exception
handler they should use for this:

\begin{code}
-- make sure we handle errors while reporting the error!
-- (e.g. evaluating the string passed to 'error' might generate
--  another error, etc.)
topHandler :: Bool -> Exception -> IO ()
topHandler bombOut err = catchException (real_handler bombOut err) (topHandler bombOut)

real_handler :: Bool -> Exception -> IO ()
real_handler bombOut ex =
  case ex of
	AsyncException StackOverflow -> reportStackOverflow bombOut
	ErrorCall s -> reportError bombOut s
	other       -> reportError bombOut (showsPrec 0 other "\n")

reportStackOverflow :: Bool -> IO ()
reportStackOverflow bombOut = do
   (hFlush stdout) `catchException` (\ _ -> return ())
   callStackOverflowHook
   if bombOut then
     stg_exit 2
    else
     return ()

reportError :: Bool -> String -> IO ()
reportError bombOut str = do
   (hFlush stdout) `catchException` (\ _ -> return ())
   let bs@(ByteArray (_,len) _) = packString str
   writeErrString addrOf_ErrorHdrHook bs len
   if bombOut then
     stg_exit 1
    else
     return ()

foreign label "ErrorHdrHook" 
        addrOf_ErrorHdrHook :: Addr

foreign import ccall "writeErrString__" 
	writeErrString :: Addr -> ByteArray Int -> Int -> IO ()

foreign import ccall "stackOverflow"
	callStackOverflowHook :: IO ()

foreign import ccall "stg_exit"
	stg_exit :: Int -> IO ()
\end{code}


A number of operations want to get at a readable or writeable handle, and fail
if it isn't:

\begin{code}
wantReadableHandle :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantReadableHandle fun handle act = 
    withHandle_ handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError -> ioError theError
      ClosedHandle 	   -> ioe_closedHandle fun handle
      SemiClosedHandle 	   -> ioe_closedHandle fun handle
      AppendHandle 	   -> ioError not_readable_error
      WriteHandle 	   -> ioError not_readable_error
      _ 		   -> act handle_
  where
   not_readable_error = 
	   IOError (Just handle) IllegalOperation fun	
		   ("handle is not open for reading")

wantWriteableHandle :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantWriteableHandle fun handle act = 
    withHandle_ handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError -> ioError theError
      ClosedHandle 	   -> ioe_closedHandle fun handle
      SemiClosedHandle 	   -> ioe_closedHandle fun handle
      ReadHandle 	   -> ioError not_writeable_error
      _ 		   -> act handle_
  where
   not_writeable_error = 
	   IOError (Just handle) IllegalOperation fun
		   ("handle is not open for writing")

wantRWHandle :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantRWHandle fun handle act = 
    withHandle_ handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError -> ioError theError
      ClosedHandle 	   -> ioe_closedHandle fun handle
      SemiClosedHandle 	   -> ioe_closedHandle fun handle
      _ 		   -> act handle_

wantSeekableHandle :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantSeekableHandle fun handle act =
    withHandle_ handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError -> ioError theError
      ClosedHandle 	   -> ioe_closedHandle fun handle
      SemiClosedHandle	   -> ioe_closedHandle fun handle
      AppendHandle  	   -> ioError not_seekable_error
      _ 		   -> act handle_
  where
   not_seekable_error = 
	   IOError (Just handle) 
	           IllegalOperation fun
		   ("handle is not seekable")

\end{code}

Internal function for creating an @IOError@ representing the
access to a closed file.

\begin{code}
ioe_closedHandle :: String -> Handle -> IO a
ioe_closedHandle fun h = ioError (IOError (Just h) IllegalOperation fun "handle is closed")
\end{code}

Internal helper functions for Concurrent Haskell implementation
of IO:

\begin{code}
#ifndef __PARALLEL_HASKELL__
mayBlock :: ForeignObj -> IO Int -> IO Int
#else
mayBlock :: Addr  -> IO Int -> IO Int
#endif

mayBlock fo act = do
   rc <- act
   case rc of
     -5 -> do  -- (possibly blocking) read
        fd <- CCALL(getFileFd) fo
        threadWaitRead fd
        CCALL(clearNonBlockingIOFlag__) fo  -- force read to happen this time.
	mayBlock fo act  -- input available, re-try
     -6 -> do  -- (possibly blocking) write
        fd <- CCALL(getFileFd) fo
        threadWaitWrite fd
        CCALL(clearNonBlockingIOFlag__) fo  -- force write to happen this time.
	mayBlock fo act  -- output possible
     -7 -> do  -- (possibly blocking) write on connected handle
        fd <- CCALL(getConnFileFd) fo
        threadWaitWrite fd
        CCALL(clearConnNonBlockingIOFlag__) fo  -- force write to happen this time.
	mayBlock fo act  -- output possible
     _ -> do
	CCALL(setNonBlockingIOFlag__) fo      -- reset file object.
	CCALL(setConnNonBlockingIOFlag__) fo  -- reset (connected) file object.
        return rc
\end{code}


\begin{code}
#ifdef __HUGS__
type FD           = Int
type Exclusive    = Int  -- really Bool
type How          = Int
type Binary       = Int
type OpenStdFlags = Int
type OpenFlags    = Int
type Readable     = Int  -- really Bool
type Flush        = Int  -- really Bool
type RC           = Int  -- standard return code

type IOFileAddr   = Addr  -- as returned from functions
type CString      = PrimByteArray
type Bytes        = PrimMutableByteArray RealWorld

#ifndef __PARALLEL_HASKELL__
type FILE_OBJ  = ForeignObj -- as passed into functions
#else
type FILE_OBJ  = Addr
#endif

foreign import ccall "libHS_cbits.so" "setBuf"                unsafe prim_setBuf           :: FILE_OBJ -> Addr -> Int -> IO ()
foreign import ccall "libHS_cbits.so" "getBufSize"            unsafe prim_getBufSize       :: FILE_OBJ -> IO Int
foreign import ccall "libHS_cbits.so" "inputReady"            unsafe prim_inputReady       :: FILE_OBJ -> Int -> IO RC
foreign import ccall "libHS_cbits.so" "fileGetc"              unsafe prim_fileGetc         :: FILE_OBJ -> IO Int
foreign import ccall "libHS_cbits.so" "fileLookAhead"         unsafe prim_fileLookAhead    :: FILE_OBJ -> IO Int
foreign import ccall "libHS_cbits.so" "readBlock"             unsafe prim_readBlock        :: FILE_OBJ -> IO Int
foreign import ccall "libHS_cbits.so" "readLine"              unsafe prim_readLine         :: FILE_OBJ -> IO Int
foreign import ccall "libHS_cbits.so" "readChar"              unsafe prim_readChar         :: FILE_OBJ -> IO Int
foreign import ccall "libHS_cbits.so" "writeFileObject"       unsafe prim_writeFileObject  :: FILE_OBJ -> Int -> IO RC
foreign import ccall "libHS_cbits.so" "filePutc"              unsafe prim_filePutc         :: FILE_OBJ -> Char -> IO RC
foreign import ccall "libHS_cbits.so" "getBufStart"           unsafe prim_getBufStart      :: FILE_OBJ -> Int -> IO Addr
foreign import ccall "libHS_cbits.so" "getWriteableBuf"       unsafe prim_getWriteableBuf  :: FILE_OBJ -> IO Addr
foreign import ccall "libHS_cbits.so" "getBufWPtr"            unsafe prim_getBufWPtr       :: FILE_OBJ -> IO Int
foreign import ccall "libHS_cbits.so" "setBufWPtr"            unsafe prim_setBufWPtr       :: FILE_OBJ -> Int -> IO ()
foreign import ccall "libHS_cbits.so" "closeFile"             unsafe prim_closeFile        :: FILE_OBJ -> Flush -> IO RC
foreign import ccall "libHS_cbits.so" "fileEOF"               unsafe prim_fileEOF           :: FILE_OBJ -> IO RC
foreign import ccall "libHS_cbits.so" "setBuffering"          unsafe prim_setBuffering      :: FILE_OBJ -> Int -> IO RC
foreign import ccall "libHS_cbits.so" "flushFile"             unsafe prim_flushFile         :: FILE_OBJ -> IO RC
foreign import ccall "libHS_cbits.so" "flushConnectedBuf"     unsafe prim_flushConnectedBuf :: FILE_OBJ -> IO RC
foreign import ccall "libHS_cbits.so" "getBufferMode"         unsafe prim_getBufferMode     :: FILE_OBJ -> IO RC
foreign import ccall "libHS_cbits.so" "seekFile_int64"        unsafe prim_seekFile_int64    :: FILE_OBJ -> Int -> Int64 -> IO RC
foreign import ccall "libHS_cbits.so" "seekFileP"             unsafe prim_seekFileP        :: FILE_OBJ -> IO RC
foreign import ccall "libHS_cbits.so" "setTerminalEcho"       unsafe prim_setTerminalEcho  :: FILE_OBJ -> Int -> IO RC
foreign import ccall "libHS_cbits.so" "getTerminalEcho"       unsafe prim_getTerminalEcho  :: FILE_OBJ -> IO RC
foreign import ccall "libHS_cbits.so" "isTerminalDevice"      unsafe prim_isTerminalDevice :: FILE_OBJ -> IO RC
foreign import ccall "libHS_cbits.so" "setConnectedTo"        unsafe prim_setConnectedTo   :: FILE_OBJ -> FILE_OBJ -> Int -> IO ()
foreign import ccall "libHS_cbits.so" "ungetChar"             unsafe prim_ungetChar        :: FILE_OBJ -> Char -> IO RC
foreign import ccall "libHS_cbits.so" "readChunk"             unsafe prim_readChunk        :: FILE_OBJ -> Addr          -> Int -> IO RC
foreign import ccall "libHS_cbits.so" "writeBuf"              unsafe prim_writeBuf         :: FILE_OBJ -> Addr -> Int -> IO RC
foreign import ccall "libHS_cbits.so" "getFileFd"             unsafe prim_getFileFd        :: FILE_OBJ -> IO FD
foreign import ccall "libHS_cbits.so" "fileSize_int64"        unsafe prim_fileSize_int64   :: FILE_OBJ -> Bytes -> IO RC
foreign import ccall "libHS_cbits.so" "getFilePosn"           unsafe prim_getFilePosn      :: FILE_OBJ -> IO Int
foreign import ccall "libHS_cbits.so" "setFilePosn"           unsafe prim_setFilePosn      :: FILE_OBJ -> Int -> IO Int
foreign import ccall "libHS_cbits.so" "getConnFileFd"         unsafe prim_getConnFileFd    :: FILE_OBJ -> IO FD
foreign import ccall "libHS_cbits.so" "allocMemory__"         unsafe prim_allocMemory__    :: Int -> IO Addr
foreign import ccall "libHS_cbits.so" "getLock"               unsafe prim_getLock          :: FD -> Exclusive -> IO RC
foreign import ccall "libHS_cbits.so" "openStdFile"           unsafe prim_openStdFile      :: FD -> OpenStdFlags -> Readable -> IO IOFileAddr
foreign import ccall "libHS_cbits.so" "openFile"              unsafe prim_openFile         :: CString -> How -> Binary -> OpenFlags -> IO IOFileAddr
foreign import ccall "libHS_cbits.so" "freeFileObject"        unsafe prim_freeFileObject    :: FILE_OBJ -> IO ()
foreign import ccall "libHS_cbits.so" "freeStdFileObject"     unsafe prim_freeStdFileObject :: FILE_OBJ -> IO ()
foreign import ccall "libHS_cbits.so" "const_BUFSIZ"          unsafe const_BUFSIZ          :: Int

foreign import ccall "libHS_cbits.so" "setConnNonBlockingIOFlag__"   unsafe prim_setConnNonBlockingIOFlag__   :: FILE_OBJ -> IO ()
foreign import ccall "libHS_cbits.so" "clearConnNonBlockingIOFlag__" unsafe prim_clearConnNonBlockingIOFlag__ :: FILE_OBJ -> IO ()
foreign import ccall "libHS_cbits.so" "setNonBlockingIOFlag__"       unsafe prim_setNonBlockingIOFlag__       :: FILE_OBJ -> IO ()
foreign import ccall "libHS_cbits.so" "clearNonBlockingIOFlag__"     unsafe prim_clearNonBlockingIOFlag__     :: FILE_OBJ -> IO ()

foreign import ccall "libHS_cbits.so" "getErrStr__"  unsafe prim_getErrStr__  :: IO Addr 
foreign import ccall "libHS_cbits.so" "getErrNo__"   unsafe prim_getErrNo__   :: IO Int  
foreign import ccall "libHS_cbits.so" "getErrType__" unsafe prim_getErrType__ :: IO Int  

#endif
\end{code}


