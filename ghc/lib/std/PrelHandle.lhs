%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelHandle]{Module @PrelHandle@}

This module defines Haskell {\em handles} and the basic operations
which are supported for them.

\begin{code}
{-# OPTIONS -fno-implicit-prelude -#include "cbits/stgio.h" #-}
#include "error.h"


module PrelHandle where

import PrelBase
import PrelArr		( newVar, readVar, writeVar, ByteArray )
import PrelRead		( Read )
import PrelList 	( span )
import PrelIOBase
import PrelMaybe	( Maybe(..) )
import PrelAddr		( Addr, nullAddr )
import PrelBounded      ()   -- get at Bounded Int instance.
import PrelNum		( toInteger )
import Ix

#ifndef __PARALLEL_HASKELL__
import PrelForeign  ( ForeignObj, makeForeignObj, writeForeignObj )
#endif

import PrelConc				-- concurrent only
\end{code}


%*********************************************************
%*							*
\subsection{Types @Handle@, @Handle__@}
%*							*
%*********************************************************

The @Handle@ and @Handle__@ types are defined in @IOBase@.

\begin{code}
{-# INLINE newHandle   #-}
{-# INLINE readHandle  #-}
{-# INLINE writeHandle #-}
newHandle   :: Handle__ -> IO Handle
readHandle  :: Handle   -> IO Handle__
writeHandle :: Handle -> Handle__ -> IO ()

#if defined(__CONCURRENT_HASKELL__)

-- Use MVars for concurrent Haskell
newHandle hc  = newMVar	hc 	>>= \ h ->
	        return (Handle h)

readHandle  (Handle h)    = takeMVar h
writeHandle (Handle h) hc = putMVar h hc

#else 

-- Use ordinary MutableVars for non-concurrent Haskell
newHandle hc  = stToIO (newVar	hc 	>>= \ h ->
		        return (Handle h))

readHandle  (Handle h)    = stToIO (readVar h)
writeHandle (Handle h) hc = stToIO (writeVar h hc)

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
    rc <- _ccall_ getLock 1 1   -- ConcHask: SAFE, won't block
    case rc of
       0 -> newHandle (mkClosedHandle__)
       1 -> do
#ifndef __CONCURRENT_HASKELL__
 	    fo <- _ccall_ openStdFile 1 1{-flush on close-} 0{-writeable-}  -- ConcHask: SAFE, won't block
#else
 	    fo <- _ccall_ openStdFile 1 (1{-flush on close-} + 128{-don't block on I/O-})
					0{-writeable-}  -- ConcHask: SAFE, won't block
#endif

#ifndef __PARALLEL_HASKELL__
            fo <- makeForeignObj fo (``&freeStdFileObject''::Addr)
#endif
	    (bm, bf_size)  <- getBMode__ fo
	    mkBuffer__ fo bf_size
	    newHandle (Handle__ fo WriteHandle bm "stdout")
       _ -> do ioError <- constructError "stdout"
               newHandle (mkErrorHandle__ ioError)
  )

stdin = unsafePerformIO (do
    rc <- _ccall_ getLock 0 0   -- ConcHask: SAFE, won't block
    case rc of
       0 -> newHandle (mkClosedHandle__)
       1 -> do
#ifndef __CONCURRENT_HASKELL__
	    fo <- _ccall_ openStdFile 0 0{-don't flush on close -} 1{-readable-}  -- ConcHask: SAFE, won't block
#else
	    fo <- _ccall_ openStdFile 0 (0{-flush on close-} + 128{-don't block on I/O-})
					1{-readable-}  -- ConcHask: SAFE, won't block
#endif

#ifndef __PARALLEL_HASKELL__
            fo <- makeForeignObj fo (``&freeStdFileObject''::Addr)
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
    rc <- _ccall_ getLock 2 1  -- ConcHask: SAFE, won't block
    case rc of
       0 -> newHandle (mkClosedHandle__)
       1 -> do
#ifndef __CONCURRENT_HASKELL__
 	    fo <- _ccall_ openStdFile 2 1{-flush on close-} 0{-writeable-} -- ConcHask: SAFE, won't block
#else
 	    fo <- _ccall_ openStdFile 2 (1{-flush on close-} + 128{-don't block on I/O-})
					0{-writeable-} -- ConcHask: SAFE, won't block
#endif

#ifndef __PARALLEL_HASKELL__
            fo <- makeForeignObj fo (``&freeStdFileObject''::Addr)
#endif
            newHandle (Handle__ fo WriteHandle NoBuffering "stderr")
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
    fo <- _ccall_ openFile f file_mode binary flush_on_close  -- ConcHask: SAFE, won't block
    if fo /= nullAddr then do
#ifndef __PARALLEL_HASKELL__
	fo  <- makeForeignObj fo ((``&freeFileObject'')::Addr)
#endif
	(bm, bf_size)  <- getBMode__ fo
        mkBuffer__ fo bf_size
	newHandle (Handle__ fo htype bm f)
      else do
	constructErrorAndFailWithInfo "openFile" f
  where
    (imo, binary) =
      case m of
        BinaryMode imo -> (imo, 1)
	TextMode imo   -> (imo, 0)

#ifndef __CONCURRENT_HASKELL__
    file_mode = file_mode'
#else
    file_mode = file_mode' + 128{-Don't block on I/O-}
#endif

    (flush_on_close, file_mode') =
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

hClose handle = do
    handle_ <- readHandle handle
    case haType__ handle_ of 
      ErrorHandle ioError -> do
	  writeHandle handle handle_
	  fail ioError
      ClosedHandle -> do
          writeHandle handle handle_
	  ioe_closedHandle "hClose" handle 
      _ -> do
          rc      <- _ccall_ closeFile (haFO__ handle_) 1{-flush if you can-}  -- ConcHask: SAFE, won't block
          {- We explicitly close a file object so that we can be told
             if there were any errors. Note that after @hClose@
             has been performed, the ForeignObj embedded in the Handle
             is still lying around in the heap, so care is taken
             to avoid closing the file object when the ForeignObj
             is finalised. (we overwrite the file ptr in the underlying
	     FileObject with a NULL as part of closeFile())
	  -}
          if rc == 0 
	   then
	      writeHandle handle (handle_{ haType__   = ClosedHandle,
					   haFO__     = nullFile__ })
           else do
	     writeHandle handle handle_
	     constructErrorAndFail "hClose"

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
hFileSize handle = do
    handle_ <- readHandle handle
    case haType__ handle_ of 
      ErrorHandle ioError -> do
	  writeHandle handle handle_
	  fail ioError
      ClosedHandle -> do
	  writeHandle handle handle_
	  ioe_closedHandle "hFileSize" handle
      SemiClosedHandle -> do
	  writeHandle handle handle_
	  ioe_closedHandle "hFileSize" handle
      other ->
          -- HACK!  We build a unique MP_INT of the right shape to hold
          -- a single unsigned word, and we let the C routine 
	  -- change the data bits
	  --
	  -- For some reason, this fails to typecheck if converted to a do
	  -- expression --SDM
          _casm_ ``%r = 1;'' >>= \(I# hack#) ->
          case int2Integer# hack# of
            result@(J# _ _ d#) -> do
                rc <- _ccall_ fileSize (haFO__ handle_) d#  -- ConcHask: SAFE, won't block
                writeHandle handle handle_
                if rc == 0 then
		   return result
                 else
		   constructErrorAndFail "hFileSize"
\end{code}

For a readable handle {\em hdl}, @hIsEOF hdl@ returns
@True@ if no further input can be taken from @hdl@ or for a
physical file, if the current I/O position is equal to the length of
the file.  Otherwise, it returns @False@.

\begin{code}
hIsEOF :: Handle -> IO Bool
hIsEOF handle = do
    handle_ <- wantReadableHandle "hIsEOF" handle
    let fo = haFO__ handle_
    rc      <- mayBlock fo (_ccall_ fileEOF fo)  -- ConcHask: UNSAFE, may block
    writeHandle handle handle_
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
        | n <= 0 -> fail (IOError (Just handle)
				  InvalidArgument
			          "hSetBuffering"
				  ("illegal buffer size " ++ showsPrec 9 n []))  -- 9 => should be parens'ified.
      _ -> do
	  handle_ <- readHandle handle
          case haType__ handle_ of
	     ErrorHandle ioError -> do
	        writeHandle handle handle_
	        fail ioError
             ClosedHandle -> do
		writeHandle handle handle_
	        ioe_closedHandle "hSetBuffering" handle
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
                rc <- mayBlock fo (_ccall_ setBuffering fo bsize) -- ConcHask: UNSAFE, may block
                if rc == 0 
		 then do
		   writeHandle handle (handle_{ haBufferMode__ = mode })
                 else do
		   -- Note: failure to change the buffer size will cause old buffer to be flushed.
		   writeHandle handle handle_
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
hFlush handle = do
    handle_ <- wantWriteableHandle "hFlush" handle
    let fo = haFO__ handle_
    rc	    <- mayBlock fo (_ccall_ flushFile fo)   -- ConcHask: UNSAFE, may block
    writeHandle handle handle_
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
hGetPosn handle = do
    handle_ <- wantSeekableHandle "hGetPosn" handle
    posn    <- _ccall_ getFilePosn (haFO__ handle_)   -- ConcHask: SAFE, won't block
    writeHandle handle handle_
    if posn /= -1 then
      return (HandlePosn handle posn)
     else
      constructErrorAndFail "hGetPosn"

hSetPosn :: HandlePosn -> IO () 
hSetPosn (HandlePosn handle posn) = do
    handle_ <- wantSeekableHandle "hSetPosn" handle -- not as silly as it looks: the handle may have been closed in the meantime.
    let fo = haFO__ handle_
    rc     <- mayBlock fo (_ccall_ setFilePosn fo posn)    -- ConcHask: UNSAFE, may block
    writeHandle handle handle_
    if rc == 0 then 
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
hSeek handle mode offset@(J# _ s# d#) =  do
    handle_ <- wantSeekableHandle "hSeek" handle
    let fo = haFO__ handle_
    rc      <- mayBlock fo (_ccall_ seekFile  fo whence (I# s#) d#)  -- ConcHask: UNSAFE, may block
    writeHandle handle handle_
    if rc == 0 then 
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
hIsOpen handle = do
    handle_ <- readHandle handle
    case haType__ handle_ of 
      ErrorHandle ioError -> do
	  writeHandle handle handle_
          fail ioError
      ClosedHandle -> do
	  writeHandle handle handle_
	  return False
      SemiClosedHandle -> do
	  writeHandle handle handle_
	  return False
      _ -> do
	  writeHandle handle handle_
	  return True

hIsClosed :: Handle -> IO Bool
hIsClosed handle = do
    handle_ <- readHandle handle
    case haType__ handle_ of 
      ErrorHandle ioError -> do
	  writeHandle handle handle_
          fail ioError
      ClosedHandle -> do
	  writeHandle handle handle_
	  return True
      _ -> do
	  writeHandle handle handle_
	  return False

{- not defined, nor exported, but mentioned
   here for documentation purposes:

    hSemiClosed :: Handle -> IO Bool
    hSemiClosed h = do
       ho <- hIsOpen h
       hc <- hIsClosed h
       return (not (ho || hc))
-}

hIsReadable :: Handle -> IO Bool
hIsReadable handle = do
    handle_ <- readHandle handle
    case haType__ handle_ of 
      ErrorHandle ioError -> do
	  writeHandle handle handle_
          fail ioError
      ClosedHandle -> do
	  writeHandle handle handle_
          ioe_closedHandle "hIsReadable" handle
      SemiClosedHandle -> do
	  writeHandle handle handle_
          ioe_closedHandle "hIsReadable" handle
      htype -> do
	  writeHandle handle handle_
	  return (isReadable htype)
  where
    isReadable ReadHandle      = True
    isReadable ReadWriteHandle = True
    isReadable _	       = False

hIsWritable :: Handle -> IO Bool
hIsWritable handle = do
    handle_ <- readHandle handle
    case haType__ handle_ of 
      ErrorHandle ioError -> do
	  writeHandle handle handle_
          fail ioError
      ClosedHandle -> do
	  writeHandle handle handle_
          ioe_closedHandle "hIsWritable" handle
      SemiClosedHandle -> do
	  writeHandle handle handle_
          ioe_closedHandle "hIsWritable" handle
      htype -> do
	  writeHandle handle handle_
	  return (isWritable htype)
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
  rc <- _ccall_ getBufferMode fo    -- ConcHask: SAFE, won't block
  case (rc::Int) of
    0  -> return (NoBuffering, 0)
    -1 -> return (LineBuffering, default_buffer_size)
    -2 -> return (BlockBuffering Nothing, default_buffer_size)
    -3 -> return (NoBuffering, 0)		-- only happens on un-stat()able files.
    n  -> return (BlockBuffering (Just n), n)
 where
   default_buffer_size :: Int
   default_buffer_size = (``BUFSIZ'' - 1)
\end{code}

Querying how a handle buffers its data:

\begin{code}
hGetBuffering :: Handle -> IO BufferMode
hGetBuffering handle = do
    handle_ <- readHandle handle
    case haType__ handle_ of 
      ErrorHandle ioError -> do
	  writeHandle handle handle_
          fail ioError
      ClosedHandle -> do
	  writeHandle handle handle_
          ioe_closedHandle "hGetBuffering" handle
      _ -> do
	  {-
	   We're being non-standard here, and allow the buffering
	   of a semi-closed handle to be queried.   -- sof 6/98
          -}
	  let v = haBufferMode__ handle_
	  writeHandle handle handle_
	  return v  -- could be stricter..

\end{code}

\begin{code}
hIsSeekable :: Handle -> IO Bool
hIsSeekable handle = do
    handle_ <- readHandle handle
    case haType__ handle_ of 
      ErrorHandle ioError -> do
	  writeHandle handle handle_
          fail ioError
      ClosedHandle -> do
	  writeHandle handle handle_
          ioe_closedHandle "hIsSeekable" handle
      SemiClosedHandle -> do
	  writeHandle handle handle_
          ioe_closedHandle "hIsSeekable" handle
      AppendHandle -> do
	  writeHandle handle handle_
	  return False
      other -> do
	  rc <- _ccall_ seekFileP (haFO__ handle_)   -- ConcHask: SAFE, won't block
	  writeHandle handle handle_
	  case rc of
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
hSetEcho hdl on = do
    isT   <- hIsTerminalDevice hdl
    if not isT
     then return ()
     else do
      handle_ <- readHandle hdl
      case haType__ handle_ of 
         ErrorHandle ioError ->  do 
            writeHandle hdl handle_
	    fail ioError
         ClosedHandle	   ->  do
            writeHandle hdl handle_
	    ioe_closedHandle "hSetEcho" hdl
         other -> do
            rc <- _ccall_ setTerminalEcho (haFO__ handle_) (if on then 1 else 0)  -- ConcHask: SAFE, won't block
	    writeHandle hdl handle_
	    if rc /= -1
	     then return ()
	     else constructErrorAndFail "hSetEcho"

hGetEcho :: Handle -> IO Bool
hGetEcho hdl = do
    isT   <- hIsTerminalDevice hdl
    if not isT
     then return False
     else do
       handle_ <- readHandle hdl
       case haType__ handle_ of 
         ErrorHandle ioError ->  do 
            writeHandle hdl handle_
	    fail ioError
         ClosedHandle	   ->  do
            writeHandle hdl handle_
	    ioe_closedHandle "hGetEcho" hdl
         other -> do
            rc <- _ccall_ getTerminalEcho (haFO__ handle_)  -- ConcHask: SAFE, won't block
	    writeHandle hdl handle_
	    case rc of
	      1 -> return True
	      0 -> return False
	      _ -> constructErrorAndFail "hSetEcho"

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice hdl = do
    handle_ <- readHandle hdl
    case haType__ handle_ of 
       ErrorHandle ioError ->  do 
            writeHandle hdl handle_
	    fail ioError
       ClosedHandle	   ->  do
            writeHandle hdl handle_
	    ioe_closedHandle "hIsTerminalDevice" hdl
       other -> do
          rc <- _ccall_ isTerminalDevice (haFO__ handle_)   -- ConcHask: SAFE, won't block
	  writeHandle hdl handle_
	  case rc of
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
hConnectHdl_ hW hR is_tty = do
  hW_ <- wantWriteableHandle "hConnectTo" hW
  hR_ <- wantReadableHandle  "hConnectTo" hR
  _ccall_ setConnectedTo (haFO__ hR_) (haFO__ hW_) is_tty  -- ConcHask: SAFE, won't block
  writeHandle hR hR_
  writeHandle hW hW_

\end{code}

As an extension, we also allow characters to be pushed back.
Like ANSI C stdio, we guarantee no more than one character of
pushback. (For unbuffered channels, the (default) push-back limit is
2 chars tho.)

\begin{code}
hUngetChar :: Handle -> Char -> IO ()
hUngetChar handle c = do
    handle_ <- wantReadableHandle "hLookAhead" handle
    rc      <- _ccall_ ungetChar (haFO__ handle_) (ord c)  -- ConcHask: SAFE, won't block
    writeHandle handle handle_
    if rc == (-1)
     then constructErrorAndFail "hUngetChar"
     else return ()

\end{code}


Hoisting files in in one go is sometimes useful, so we support
this as an extension:

\begin{code}
-- in one go, read file into an externally allocated buffer.
slurpFile :: FilePath -> IO (Addr, Int)
slurpFile fname = do
  hdl <- openFile fname ReadMode
  sz  <- hFileSize hdl
  if sz > toInteger (maxBound::Int) then 
    fail (userError "slurpFile: file too big")
   else do
     let sz_i = fromInteger sz
     chunk <- _ccall_ allocMemory__ (sz_i::Int)
     if chunk == nullAddr 
      then do
        hClose hdl
        constructErrorAndFail "slurpFile"
      else do
        handle_ <- readHandle hdl
        let fo = haFO__ handle_
	rc      <- mayBlock fo (_ccall_ readChunk fo chunk sz_i)    -- ConcHask: UNSAFE, may block.
        writeHandle hdl handle_
	hClose hdl
        if rc < 0
	 then constructErrorAndFail "slurpFile"
	 else return (chunk, rc)

hFillBufBA :: Handle -> ByteArray Int -> Int -> IO Int
hFillBufBA handle buf sz
  | sz <= 0 = fail (IOError (Just handle)
			    InvalidArgument
		            "hFillBufBA"
			    ("illegal buffer size " ++ showsPrec 9 sz []))  -- 9 => should be parens'ified.
  | otherwise = do
    handle_ <- wantReadableHandle "hFillBufBA" handle
    let fo  = haFO__ handle_
    rc      <- mayBlock fo (_ccall_ readChunk fo buf sz)    -- ConcHask: UNSAFE, may block.
    writeHandle handle handle_
    if rc >= 0
     then return rc
     else constructErrorAndFail "hFillBufBA"

hFillBuf :: Handle -> Addr -> Int -> IO Int
hFillBuf handle buf sz
  | sz <= 0 = fail (IOError (Just handle)
			    InvalidArgument
		            "hFillBuf"
			    ("illegal buffer size " ++ showsPrec 9 sz []))  -- 9 => should be parens'ified.
  | otherwise = do
    handle_ <- wantReadableHandle "hFillBuf" handle
    let fo  = haFO__ handle_
    rc      <- mayBlock fo (_ccall_ readChunk fo buf sz)    -- ConcHask: UNSAFE, may block.
    writeHandle handle handle_
    if rc >= 0
     then return rc
     else constructErrorAndFail "hFillBuf"

\end{code}

The @hPutBuf hdl buf len@ action writes an already packed sequence of
bytes to the file/channel managed by @hdl@ - non-standard.

\begin{code}
hPutBuf :: Handle -> Addr -> Int -> IO ()
hPutBuf handle buf len = do
    handle_ <- wantWriteableHandle "hPutBuf" handle
    let fo  = haFO__ handle_
    rc      <- mayBlock fo (_ccall_ writeBuf fo buf len)  -- ConcHask: UNSAFE, may block.
    writeHandle handle handle_
    if rc == 0
     then return ()
     else constructErrorAndFail "hPutBuf"

hPutBufBA :: Handle -> ByteArray Int -> Int -> IO ()
hPutBufBA handle buf len = do
    handle_ <- wantWriteableHandle "hPutBufBA" handle
    let fo = haFO__ handle_
    rc      <- mayBlock fo (_ccall_ writeBufBA fo buf len)  -- ConcHask: UNSAFE, may block.
    writeHandle handle handle_
    if rc == 0
     then return ()
     else constructErrorAndFail "hPutBuf"
\end{code}

Sometimes it's useful to get at the file descriptor that
the Handle contains..

\begin{code}
getHandleFd :: Handle -> IO Int
getHandleFd handle = do
    handle_ <- readHandle handle
    case (haType__ handle_) of
      ErrorHandle ioError -> do
	  writeHandle handle handle_
          fail ioError
      ClosedHandle -> do
	  writeHandle handle handle_
	  ioe_closedHandle "getHandleFd" handle
      _ -> do
          fd <- _ccall_ getFileFd (haFO__ handle_)
	  writeHandle handle handle_
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
   (fs,[]) -> Nothing
   (fs,_)  -> Just fs

\end{code}

A number of operations want to get at a readable or writeable handle, and fail
if it isn't:

\begin{code}
wantReadableHandle :: String -> Handle -> IO Handle__
wantReadableHandle fun handle = do
    handle_ <- readHandle handle
    case haType__ handle_ of 
      ErrorHandle ioError -> do
	  writeHandle handle handle_
          fail ioError
      ClosedHandle -> do
	  writeHandle handle handle_
	  ioe_closedHandle fun handle
      SemiClosedHandle -> do
	  writeHandle handle handle_
	  ioe_closedHandle fun handle
      AppendHandle -> do
	  writeHandle handle handle_
	  fail not_readable_error
      WriteHandle -> do
	  writeHandle handle handle_
	  fail not_readable_error
      other -> return handle_
  where
   not_readable_error = 
	   IOError (Just handle) IllegalOperation fun	
		   ("handle is not open for reading")

wantWriteableHandle :: String -> Handle -> IO Handle__
wantWriteableHandle fun handle = do
    handle_ <- readHandle handle
    case haType__ handle_ of 
      ErrorHandle ioError -> do
	  writeHandle handle handle_
          fail ioError
      ClosedHandle -> do
	  writeHandle handle handle_
	  ioe_closedHandle fun handle
      SemiClosedHandle -> do
	  writeHandle handle handle_
	  ioe_closedHandle fun handle
      ReadHandle -> do
	  writeHandle handle handle_
	  fail not_writeable_error
      other -> return handle_
  where
   not_writeable_error = 
	   IOError (Just handle) IllegalOperation fun
		   ("handle is not open for writing")

wantSeekableHandle :: String -> Handle -> IO Handle__
wantSeekableHandle fun handle = do
    handle_ <- readHandle handle
    case haType__ handle_ of 
      ErrorHandle ioError -> do
	  writeHandle handle handle_
          fail ioError
      ClosedHandle -> do
	  writeHandle handle handle_
	  ioe_closedHandle fun handle
      SemiClosedHandle -> do
	  writeHandle handle handle_
	  ioe_closedHandle fun handle
      AppendHandle -> do
	  writeHandle handle handle_
	  fail not_seekable_error
      _ -> return handle_
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
ioe_closedHandle fun h = fail (IOError (Just h) IllegalOperation fun "handle is closed")
\end{code}

Internal helper functions for Concurrent Haskell implementation
of IO:

\begin{code}
#ifndef __PARALLEL_HASKELL__
mayBlock :: ForeignObj -> IO Int -> IO Int
#else
mayBlock :: Addr  -> IO Int -> IO Int
#endif

#ifndef __CONCURRENT_HASKELL__
mayBlock  _ act = act
#else
mayBlock fo act = do
   rc <- act
   case rc of
     -5 -> do  -- (possibly blocking) read
        fd <- _ccall_ getFileFd fo
        threadWaitRead fd
        _ccall_ clearNonBlockingIOFlag__ fo  -- force read to happen this time.
	mayBlock fo act  -- input available, re-try
     -6 -> do  -- (possibly blocking) write
        fd <- _ccall_ getFileFd fo
        threadWaitWrite fd
        _ccall_ clearNonBlockingIOFlag__ fo  -- force write to happen this time.
	mayBlock fo act  -- output possible
     -7 -> do  -- (possibly blocking) write on connected handle
        fd <- _ccall_ getConnFileFd fo
        threadWaitWrite fd
        _ccall_ clearConnNonBlockingIOFlag__ fo  -- force write to happen this time.
	mayBlock fo act  -- output possible
     _ -> do
	_ccall_ setNonBlockingIOFlag__ fo      -- reset file object.
	_ccall_ setConnNonBlockingIOFlag__ fo  -- reset (connected) file object.
        return rc

#endif
\end{code}


