%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[IOHandle]{Module @IOHandle@}

This module defines Haskell {\em handles} and the basic operations
which are supported for them.

\begin{code}
{-# OPTIONS -fno-implicit-prelude -#include "cbits/stgio.h" #-}
#include "error.h"


module IOHandle where

import ST
import UnsafeST
import STBase
import ArrBase	( ByteArray(..) )
import PrelRead	( Read )
import Ix
import IOBase
import PrelTup
import PrelBase
import GHC
import Foreign  ( ForeignObj, Addr, makeForeignObj, writeForeignObj )
import PrelList (span)
#if defined(__CONCURRENT_HASKELL__)
import ConcBase
#endif
\end{code}


%*********************************************************
%*							*
\subsection{Types @FilePath@, @Handle@, @Handle__@}
%*							*
%*********************************************************

The @Handle@ and @Handle__@ types are defined in @IOBase@.

\begin{code}
type FilePath = String

{-# INLINE newHandle   #-}
{-# INLINE readHandle  #-}
{-# INLINE writeHandle #-}
newHandle   :: Handle__ -> IO Handle
readHandle  :: Handle   -> IO Handle__
writeHandle :: Handle -> Handle__ -> IO ()

#if defined(__CONCURRENT_HASKELL__)
newHandle   = newMVar
readHandle  = takeMVar
writeHandle = putMVar
#else 
newHandle v     = stToIO (newVar   v)
readHandle h    = stToIO (readVar  h)
writeHandle h v = stToIO (writeVar h v)
#endif

\end{code}

%*********************************************************
%*							*
\subsection{Functions}
%*							*
%*********************************************************

\begin{code}
#ifndef PAR
filePtr :: Handle__ -> ForeignObj
#else
filePtr :: Handle__ -> Addr
#endif
filePtr (SemiClosedHandle fp _)  = fp
filePtr (ReadHandle fp _ _)	 = fp
filePtr (WriteHandle fp _ _)	 = fp
filePtr (AppendHandle fp _ _)	 = fp
filePtr (ReadWriteHandle fp _ _) = fp

bufferMode :: Handle__ -> Maybe BufferMode
bufferMode (ReadHandle _ m _)      = m
bufferMode (WriteHandle _ m _)     = m
bufferMode (AppendHandle _ m _)    = m
bufferMode (ReadWriteHandle _ m _) = m

markHandle :: Handle__ -> Handle__
markHandle h@(ReadHandle fp m b)
  | b = h
  | otherwise = ReadHandle fp m True
markHandle h@(WriteHandle fp m b)
  | b = h
  | otherwise = WriteHandle fp m True
markHandle h@(AppendHandle fp m b)
  | b = h
  | otherwise = AppendHandle fp m True
markHandle h@(ReadWriteHandle fp m b)
  | b = h
  | otherwise = ReadWriteHandle fp m True
\end{code}

-------------------------------------------

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

stdin = unsafePerformPrimIO (
    _ccall_ getLock (``stdin''::Addr) 0		>>= \ rc ->
    (case rc of
       0 -> new_handle ClosedHandle
       1 -> 
#ifndef PAR
            makeForeignObj (``stdin''::Addr) (``&freeStdChannel''::Addr) >>= \ fp ->
	    new_handle (ReadHandle fp Nothing False)
#else
	    new_handle (ReadHandle ``stdin'' Nothing False)
#endif
       _ -> constructError "stdin"		>>= \ ioError -> 
            new_handle (ErrorHandle ioError)
    )						>>= \ handle ->
    returnPrimIO handle
  )
  where
    new_handle x = ioToST (newHandle x)

stdout = unsafePerformPrimIO (
    _ccall_ getLock (``stdout''::Addr) 1	>>= \ rc ->
    (case rc of
       0 -> new_handle ClosedHandle
       1 -> 
#ifndef PAR
            makeForeignObj (``stdout''::Addr) (``&freeStdChannel''::Addr) >>= \ fp ->
	    new_handle (WriteHandle fp Nothing False)
#else
	    new_handle (WriteHandle ``stdout'' Nothing False)
#endif
       _ -> constructError "stdout"		>>= \ ioError -> 
            new_handle (ErrorHandle ioError)
    )						>>= \ handle ->
    returnPrimIO handle
  )
  where
    new_handle x = ioToST (newHandle x)

stderr = unsafePerformPrimIO (
    _ccall_ getLock (``stderr''::Addr) 1	>>= \ rc ->
    (case rc of
       0 -> new_handle ClosedHandle
       1 -> 
#ifndef PAR
            makeForeignObj (``stderr''::Addr) (``&freeStdChannel''::Addr) >>= \ fp ->
            new_handle (WriteHandle fp (Just NoBuffering) False)	
#else
            new_handle (WriteHandle ``stderr'' (Just NoBuffering) False)	
#endif
       _ -> constructError "stderr"		>>= \ ioError -> 
            new_handle (ErrorHandle ioError)
    )						>>= \ handle ->
    returnPrimIO handle
  )
  where
    new_handle x = ioToST (newHandle x)
\end{code}

%*********************************************************
%*							*
\subsection[OpeningClosing]{Opening and Closing Files}
%*							*
%*********************************************************

\begin{code}
data IOMode      =  ReadMode | WriteMode | AppendMode | ReadWriteMode
                    deriving (Eq, Ord, Ix, Enum, Read, Show)

openFile :: FilePath -> IOMode -> IO Handle

openFile f m = 
    stToIO (_ccall_ openFile f m')		             >>= \ ptr ->
    if ptr /= ``NULL'' then
#ifndef PAR
        makeForeignObj ptr ((``&freeFile'')::Addr)   `thenIO_Prim` \ fp ->
        newHandle (htype fp Nothing False)
#else
        newHandle (htype ptr Nothing False)
#endif
    else
	stToIO (constructError "openFile")	    >>= \ ioError@(IOError hn iot msg) -> 
	let
	    improved_error -- a HACK, I guess
	      = case iot of
		  AlreadyExists    -> IOError hn AlreadyExists    (msg ++ ": " ++ f)
		  NoSuchThing      -> IOError hn NoSuchThing      (msg ++ ": " ++ f)
		  PermissionDenied -> IOError hn PermissionDenied (msg ++ ": " ++ f)
		  _		   -> ioError
	in
        fail improved_error
  where
    m' = case m of 
           ReadMode      -> "r"
           WriteMode     -> "w"
           AppendMode    -> "a"
           ReadWriteMode -> "r+"

    htype = case m of 
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
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype >>
	  fail ioError
      ClosedHandle -> 
          writeHandle handle htype		    >>
	  ioe_closedHandle handle
      SemiClosedHandle fp (buf,_) ->
          (if buf /= ``NULL'' then
	      _ccall_ free buf
           else			    
              returnPrimIO ())			    `thenIO_Prim` \ () ->
	      _casm_ `` %r = (char *)%0; '' fp      `thenIO_Prim` \ fp_a ->
              if fp_a /= (``NULL''::Addr) then -- Under what condition can this be NULL?
                _ccall_ closeFile fp		    `thenIO_Prim` \ rc ->
	          {- We explicitly close a file object so that we can be told
	             if there were any errors. Note that after @hClose@
	             has been performed, the ForeignObj embedded in the Handle
                     is still lying around in the heap, so care is taken
                     to avoid closing the file object when the ForeignObj
	             is finalised.  -}
                if rc == 0 then 
#ifndef PAR
		  -- Mark the foreign object data value as gone to the finaliser (freeFile())
		  writeForeignObj fp ``NULL''       `thenIO_Prim` \ () ->
#endif
		  writeHandle handle ClosedHandle
                else
		  writeHandle handle htype >>
	          constructErrorAndFail "hClose"

              else			    
                  writeHandle handle htype
      other -> 
	  let fp = filePtr other in
          _ccall_ closeFile fp	    `thenIO_Prim` \ rc ->
          if rc == 0 then 
#ifndef PAR
		  -- Mark the foreign object data
		  writeForeignObj fp ``NULL''       `thenIO_Prim` \ () ->
#endif
	      writeHandle handle ClosedHandle
          else
 	      writeHandle handle htype >>
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
hFileSize handle =
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype			    >>
	  fail ioError
      ClosedHandle -> 
	  writeHandle handle htype			    >>
	  ioe_closedHandle handle
      SemiClosedHandle _ _ -> 
	  writeHandle handle htype			    >>
	  ioe_closedHandle handle
      other ->
          -- HACK!  We build a unique MP_INT of the right shape to hold
          -- a single unsigned word, and we let the C routine change the data bits
          _casm_ ``%r = 1;''			    `thenIO_Prim` \ (I# hack#) ->
          case int2Integer# hack# of
            result@(J# _ _ d#) ->
		let
		    bogus_bounds = (error "fileSize"::(Int,Int))
		in
                _ccall_ fileSize (filePtr other) (ByteArray bogus_bounds d#)
                                                    `thenIO_Prim` \ rc ->
               writeHandle handle htype		    >>
               if rc == 0 then
		   return result
               else
		    constructErrorAndFail "hFileSize"
\end{code}

For a readable handle {\em hdl}, computation $hIsEOF hdl$ returns
$True$ if no further input can be taken from {\em hdl} or for a
physical file, if the current I/O position is equal to the length of
the file.  Otherwise, it returns $False$.

\begin{code}
hIsEOF :: Handle -> IO Bool
hIsEOF handle =
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
	  fail ioError
      ClosedHandle -> 
	  writeHandle handle htype		    >>
	  ioe_closedHandle handle
      SemiClosedHandle _ _ -> 
	  writeHandle handle htype		    >>
	  ioe_closedHandle handle
      WriteHandle _ _ _ -> 
	  writeHandle handle htype		    >>
          fail (IOError (Just handle) IllegalOperation "handle is not open for reading")
      AppendHandle _ _ _ -> 
	  writeHandle handle htype		    >>
          fail (IOError (Just handle) IllegalOperation "handle is not open for reading")
      other -> 
          _ccall_ fileEOF (filePtr other)	    `thenIO_Prim` \ rc ->
	  writeHandle handle (markHandle htype)	    >>
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
      (BlockBuffering (Just n)) 
        | n <= 0 -> fail (IOError (Just handle) InvalidArgument "illegal buffer size")
      other ->
	  readHandle handle			    >>= \ htype ->
          if isMarked htype then
              writeHandle handle htype		    >>
              fail (IOError (Just handle) 
			    UnsupportedOperation 
			    "can't set buffering for a dirty handle")
          else
              case htype of
	        ErrorHandle ioError ->
	            writeHandle handle htype	    >>
	            fail ioError
                ClosedHandle ->
		    writeHandle handle htype	    >>
	            ioe_closedHandle handle
                SemiClosedHandle _ _ ->
		    writeHandle handle htype	    >>
	            ioe_closedHandle handle
                other ->
                    _ccall_ setBuffering (filePtr other) bsize
						    `thenIO_Prim` \ rc -> 
                    if rc == 0 then
                        writeHandle handle ((hcon other) (filePtr other) (Just mode) True)
						    >>
		        return ()
                    else
			writeHandle handle htype	 >>
		        constructErrorAndFail "hSetBuffering"
		
  where
    isMarked :: Handle__ -> Bool
    isMarked (ReadHandle fp m b) = b
    isMarked (WriteHandle fp m b) = b
    isMarked (AppendHandle fp m b) = b
    isMarked (ReadWriteHandle fp m b) = b

    bsize :: Int
    bsize = case mode of
              NoBuffering -> 0
              LineBuffering -> -1
              BlockBuffering Nothing -> -2
              BlockBuffering (Just n) -> n

#ifndef PAR
    hcon :: Handle__ -> (ForeignObj -> (Maybe BufferMode) -> Bool -> Handle__)
#else
    hcon :: Handle__ -> (Addr -> (Maybe BufferMode) -> Bool -> Handle__)
#endif
    hcon (ReadHandle _ _ _) = ReadHandle
    hcon (WriteHandle _ _ _) = WriteHandle
    hcon (AppendHandle _ _ _) = AppendHandle
    hcon (ReadWriteHandle _ _ _) = ReadWriteHandle
\end{code}

Computation $flush hdl$ causes any items buffered for output in handle
{\em hdl} to be sent immediately to the operating system.

\begin{code}
hFlush :: Handle -> IO () 
hFlush handle = 
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
	  fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      other ->
	  _ccall_ flushFile (filePtr other)	    `thenIO_Prim` \ rc ->
	  writeHandle handle (markHandle htype)   >>
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
data HandlePosn = HandlePosn Handle Int

data SeekMode    =  AbsoluteSeek | RelativeSeek | SeekFromEnd
                    deriving (Eq, Ord, Ix, Enum, Read, Show)
\end{code}

Computation $hGetPosn hdl$ returns the current I/O
position of {\em hdl} as an abstract position.  Computation
$hSetPosn p$ sets the position of {\em hdl}
to a previously obtained position {\em p}.

\begin{code}
hGetPosn :: Handle -> IO HandlePosn
hGetPosn handle = 
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
	  fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      other -> 
          _ccall_ getFilePosn (filePtr other)      `thenIO_Prim` \ posn ->
          writeHandle handle htype		    >>
          if posn /= -1 then
	      return (HandlePosn handle posn)
          else
	      constructErrorAndFail "hGetPosn"

hSetPosn :: HandlePosn -> IO () 
hSetPosn (HandlePosn handle posn) = 
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
	  fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      AppendHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  fail (IOError (Just handle) IllegalOperation "handle is not seekable")
      other -> 
	  _ccall_ setFilePosn (filePtr other) posn `thenIO_Prim` \ rc ->
	  writeHandle handle (markHandle htype)    >>
               if rc == 0 then 
		   return ()
               else
		   constructErrorAndFail "hSetPosn"
\end{code}

Computation $hSeek hdl mode i$ sets the position of handle
{\em hdl} depending on $mode$.  If {\em mode} is
\begin{itemize}
\item[{\bf AbsoluteSeek}] The position of {\em hdl} is set to {\em i}.
\item[{\bf RelativeSeek}] The position of {\em hdl} is set to offset {\em i} from
the current position.
\item[{\bf SeekToEnd}] The position of {\em hdl} is set to offset {\em i} from
the end of the file.
\item[{\bf SeekFromBeginning}] The position of {\em hdl} is set to offset {\em i} from
the beginning of the file.
\end{itemize}

Some handles may not be seekable $hIsSeekable$, or only support a
subset of the possible positioning operations (e.g. it may only be
possible to seek to the end of a tape, or to a positive offset from
the beginning or current position).

It is not possible to set a negative I/O position, or for a physical
file, an I/O position beyond the current end-of-file. 

\begin{code}
hSeek :: Handle -> SeekMode -> Integer -> IO () 
hSeek handle mode offset@(J# _ s# d#) = 
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
	  fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      AppendHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  fail (IOError (Just handle) IllegalOperation "handle is not seekable")
      other -> 
	  _ccall_ seekFile (filePtr other) whence (I# s#) (ByteArray (0,0) d#)
                                        	    `thenIO_Prim` \ rc ->
	  writeHandle handle (markHandle htype)   >>
               if rc == 0 then 
		   return ()
               else
		    constructErrorAndFail "hSeek"
  where
    whence :: Int
    whence = case mode of
               AbsoluteSeek -> ``SEEK_SET''
               RelativeSeek -> ``SEEK_CUR''
               SeekFromEnd -> ``SEEK_END''
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
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
          fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
	  return False
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
	  return False
      other ->
	  writeHandle handle htype		    >>
	  return True

hIsClosed :: Handle -> IO Bool
hIsClosed handle = 
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
          fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
	  return True
      other ->
	  writeHandle handle htype		    >>
	  return False

hIsReadable :: Handle -> IO Bool
hIsReadable handle = 
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
          fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      other ->
	  writeHandle handle htype		    >>
	  return (isReadable other)
  where
    isReadable (ReadHandle _ _ _) = True
    isReadable (ReadWriteHandle _ _ _) = True
    isReadable _ = False

hIsWritable :: Handle -> IO Bool
hIsWritable handle = 
    readHandle handle			    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype	    >>
          fail ioError
      ClosedHandle ->
	  writeHandle handle htype	    >>
          ioe_closedHandle handle
      SemiClosedHandle _ _ ->
	  writeHandle handle htype	    >>
          ioe_closedHandle handle
      other ->
	  writeHandle handle htype	    >>
	  return (isWritable other)
  where
    isWritable (AppendHandle _ _ _) = True
    isWritable (WriteHandle _ _ _) = True
    isWritable (ReadWriteHandle _ _ _) = True
    isWritable _ = False

getBufferMode :: Handle__ -> PrimIO Handle__
getBufferMode htype =
    case bufferMode htype of
      Just x -> returnPrimIO htype
      Nothing ->
	_ccall_ getBufferMode (filePtr htype)	    `thenPrimIO` \ rc ->
	let 
	    mode = 
		case rc of
                  0  -> Just NoBuffering
                  -1 -> Just LineBuffering
	          -2 -> Just (BlockBuffering Nothing)
                  -3 -> Nothing
                  n  -> Just (BlockBuffering (Just n))
	in
	returnPrimIO (case htype of
	  ReadHandle      fp _ b -> ReadHandle      fp mode b
	  WriteHandle     fp _ b -> WriteHandle     fp mode b
	  AppendHandle    fp _ b -> AppendHandle    fp mode b
	  ReadWriteHandle fp _ b -> ReadWriteHandle fp mode b)

hIsBlockBuffered :: Handle -> IO (Bool,Maybe Int)
hIsBlockBuffered handle =
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
          fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      other ->
          getBufferMode other			    `thenIO_Prim` \ other ->
          case bufferMode other of
            Just (BlockBuffering size) ->
	        writeHandle handle other	    >>
                return (True, size)
            Just _ ->
	        writeHandle handle other	    >>
                return (False, Nothing)
    	    Nothing -> 
		constructErrorAndFail "hIsBlockBuffered"

hIsLineBuffered :: Handle -> IO Bool
hIsLineBuffered handle =
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
          fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      other ->
	  getBufferMode other			    `thenIO_Prim` \ other ->
          case bufferMode other of
            Just LineBuffering ->
	        writeHandle handle other	    >>
                return True
            Just _ ->
	        writeHandle handle other	    >>
                return False
    	    Nothing -> 
		constructErrorAndFail "hIsLineBuffered"

hIsNotBuffered :: Handle -> IO Bool
hIsNotBuffered handle =
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
          fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      other ->
	  getBufferMode other			    `thenIO_Prim` \ other ->
          case bufferMode other of
            Just NoBuffering ->
	        writeHandle handle other	    >>
                return True
            Just _ ->
	        writeHandle handle other	    >>
                return False
    	    Nothing -> 
		constructErrorAndFail "hIsNotBuffered"

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering handle =
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
          fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      other ->
	  getBufferMode other			    `thenIO_Prim` \ other ->
          case bufferMode other of
            Just v ->
	        writeHandle handle other            >>
                return v
    	    Nothing -> 
		constructErrorAndFail "hGetBuffering"

hIsSeekable :: Handle -> IO Bool
hIsSeekable handle = 
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
          fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
          ioe_closedHandle handle
      AppendHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  return False
      other ->
	  _ccall_ seekFileP (filePtr other)   	    `thenIO_Prim` \ rc ->
	  writeHandle handle htype		    >>
	  case rc of
            0 -> return False
            1 -> return True
            _ -> constructErrorAndFail "hIsSeekable"
\end{code}


%*********************************************************
%*							*
\subsection{Miscellaneous}
%*							*
%*********************************************************

These two functions are meant to get things out of @IOErrors@.  They don't!

\begin{code}
ioeGetFileName        :: IOError -> Maybe FilePath
ioeGetErrorString     :: IOError -> String
ioeGetHandle          :: IOError -> Maybe Handle

ioeGetHandle   (IOError h _ _)   = h
ioeGetErrorString (IOError _ iot str) =
 case iot of
   EOF -> "end of file"
   _   -> str

ioeGetFileName (IOError _ _ str) = 
 case span (/=':') str of
   (fs,[]) -> Nothing
   (fs,_)  -> Just fs

\end{code}

Internal function for creating an @IOError@ representing the
access of a closed file.

\begin{code}

ioe_closedHandle :: Handle -> IO a
ioe_closedHandle h = fail (IOError (Just h) IllegalOperation "handle is closed")

\end{code}
