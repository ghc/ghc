{-
%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

This module defines Haskell {\em handles} and the basic operations
which are supported for them.
-}

#include "error.h"

module GHCio where

import GHCbase
import qualified GHCps ( unpackPS, packCString )
import Ix (Ix(..))

---------------------------------
infixr 1 `stThen`

-- a useful little number for doing _ccall_s in IO-land:

stThen :: PrimIO a -> (a -> IO b) -> IO b
{-# INLINE stThen   #-}

stThen (ST m) k = IO $ ST $ \ s ->
    case (m s)     of { (m_res, new_s)    ->
    case (k m_res) of { (IO (ST k_m_res)) ->
    k_m_res new_s }}

---------------------------------
-- this one didn't make it into the 1.3 defn

-- The construct $try comp$ exposes errors which occur within a
-- computation, and which are not fully handled.  It always succeeds.

tryIO :: IO a -> IO (Either IOError a) 
tryIO p = catch (p >>= (return . Right)) (return . Left)

---------------------------------

data Handle__
  = ErrorHandle		IOError
  | ClosedHandle
  | SemiClosedHandle	Addr (Addr, Int)
  | ReadHandle		Addr (Maybe BufferMode) Bool
  | WriteHandle		Addr (Maybe BufferMode) Bool
  | AppendHandle	Addr (Maybe BufferMode) Bool
  | ReadWriteHandle	Addr (Maybe BufferMode) Bool

instance Eq Handle{-partain:????-}

{-# INLINE newHandle   #-}
{-# INLINE readHandle  #-}
{-# INLINE writeHandle #-}

newHandle   :: Handle__ -> IO Handle
readHandle  :: Handle   -> IO Handle__
writeHandle :: Handle -> Handle__ -> IO ()

#if defined(__CONCURRENT_HASKELL__)

type Handle = MVar Handle__

newHandle   = newMVar
readHandle  = takeMVar
writeHandle = putMVar

#else
type Handle = MutableVar RealWorld Handle__

newHandle v     = stToIO (newVar   v)
readHandle h    = stToIO (readVar  h)
writeHandle h v = stToIO (writeVar h v)

#endif {- __CONCURRENT_HASKELL__ -}

type FilePath = String

filePtr :: Handle__ -> Addr
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

-------------------------------------------

stdin, stdout, stderr :: Handle

stdin = unsafePerformPrimIO (
    _ccall_ getLock (``stdin''::Addr) 0		>>= \ rc ->
    (case rc of
       0 -> new_handle ClosedHandle
       1 -> new_handle (ReadHandle ``stdin'' Nothing False)
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
       1 -> new_handle (WriteHandle ``stdout'' Nothing False)
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
       1 -> new_handle (WriteHandle ``stderr'' (Just NoBuffering) False)	
       _ -> constructError "stderr"		>>= \ ioError -> 
            new_handle (ErrorHandle ioError)
    )						>>= \ handle ->
    returnPrimIO handle
  )
  where
    new_handle x = ioToST (newHandle x)
{-
\end{code}

Three handles are allocated during program initialisation.  The first
two manage input or output from the Haskell program's standard input
or output channel respectively.  The third manages output to the
standard error channel. These handles are initially open.

\subsubsection[OpeningClosing]{Opening and Closing Files}

\begin{code}
-}
data IOMode      =  ReadMode | WriteMode | AppendMode | ReadWriteMode
                    deriving (Eq, Ord, Ix, Enum, Read, Show)

openFile :: FilePath -> IOMode -> IO Handle

openFile f m = 
    stToIO (_ccall_ openFile f m')		    >>= \ ptr ->
    if ptr /= ``NULL'' then
        newHandle (htype ptr Nothing False)
    else
	stToIO (constructError "openFile")	    >>= \ ioError -> 
	let
	    improved_error -- a HACK, I guess
	      = case ioError of
		  AlreadyExists    msg -> AlreadyExists	   (msg ++ ": " ++ f)
		  NoSuchThing      msg -> NoSuchThing	   (msg ++ ": " ++ f)
		  PermissionDenied msg -> PermissionDenied (msg ++ ": " ++ f)
		  _		       -> ioError
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
{-
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
-}
hClose :: Handle -> IO ()

hClose handle =
    readHandle handle				    >>= \ htype ->
    writeHandle handle ClosedHandle		    >>
    case htype of 
      ErrorHandle ioError ->
	  fail ioError
      ClosedHandle -> 
	  fail (IllegalOperation "handle is closed")
      SemiClosedHandle fp (buf,_) ->
          (if buf /= ``NULL'' then
	      _ccall_ free buf
           else			    
              returnPrimIO ())			    `stThen` \ () ->
          if fp /= ``NULL'' then
              _ccall_ closeFile fp		    `stThen` \ rc ->
              if rc == 0 then 
	          return ()
              else
	          constructErrorAndFail "hClose"
          else			    
              return ()
      other -> 
          _ccall_ closeFile (filePtr other)	    `stThen` \ rc ->
          if rc == 0 then 
	      return ()
          else
	      constructErrorAndFail "hClose"
{-
\end{code}

Computation $hClose hdl$ makes handle {\em hdl} closed.  Before the
computation finishes, any items buffered for output and not already
sent to the operating system are flushed as for $flush$.

\subsubsection[EOF]{Detecting the End of Input}

\begin{code}
-}
hFileSize :: Handle -> IO Integer
hFileSize handle =
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype			    >>
	  fail ioError
      ClosedHandle -> 
	  writeHandle handle htype			    >>
          fail (IllegalOperation "handle is closed")
      SemiClosedHandle _ _ -> 
	  writeHandle handle htype			    >>
          fail (IllegalOperation "handle is closed")
      other ->
          -- HACK!  We build a unique MP_INT of the right shape to hold
          -- a single unsigned word, and we let the C routine change the data bits
          _casm_ ``%r = 1;''			    `stThen` \ (I# hack#) ->
          case int2Integer# hack# of
            result@(J# _ _ d#) ->
		let
		    bogus_bounds = (error "fileSize"::(Int,Int))
		in
                _ccall_ fileSize (filePtr other) (ByteArray bogus_bounds d#)
                                                    `stThen` \ rc ->
               writeHandle handle htype		    >>
               if rc == 0 then
		   return result
               else
		    constructErrorAndFail "hFileSize"
{-
\end{code}

For a handle {\em hdl} which attached to a physical file, $hFileSize
hdl$ returns the size of {\em hdl} in terms of the number of items
which can be read from {\em hdl}.

\begin{code}
-}
hIsEOF :: Handle -> IO Bool
hIsEOF handle =
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
	  fail ioError
      ClosedHandle -> 
	  writeHandle handle htype		    >>
          fail (IllegalOperation "handle is closed")
      SemiClosedHandle _ _ -> 
	  writeHandle handle htype		    >>
          fail (IllegalOperation "handle is closed")
      WriteHandle _ _ _ -> 
	  writeHandle handle htype		    >>
          fail (IllegalOperation "handle is not open for reading")
      AppendHandle _ _ _ -> 
	  writeHandle handle htype		    >>
          fail (IllegalOperation "handle is not open for reading")
      other -> 
          _ccall_ fileEOF (filePtr other)	    `stThen` \ rc ->
	  writeHandle handle (markHandle htype)	    >>
	  case rc of
            0 -> return False
            1 -> return True
            _ -> constructErrorAndFail "hIsEOF"

isEOF :: IO Bool
isEOF = hIsEOF stdin
{-
\end{code}

For a readable handle {\em hdl}, computation $hIsEOF hdl$ returns
$True$ if no further input can be taken from {\em hdl} or for a
physical file, if the current I/O position is equal to the length of
the file.  Otherwise, it returns $False$.

\subsubsection[Buffering]{Buffering Operations}

Three kinds of buffering are supported: line-buffering, 
block-buffering or no-buffering.  These modes have the following effects.
For output, items are written out from the internal buffer 
according to the buffer mode:
\begin{itemize}
\item[line-buffering]  the entire output buffer is written
out whenever a newline is output, the output buffer overflows, 
a flush is issued, or the handle is closed.

\item[block-buffering] the entire output buffer is written out whenever 
it overflows, a flush is issued, or the handle
is closed.

\item[no-buffering] output is written immediately, and never stored
in the output buffer.
\end{itemize}

The output buffer is emptied as soon as it has been written out.

Similarly, input occurs according to the buffer mode for handle {\em hdl}.
\begin{itemize}
\item[line-buffering] when the input buffer for {\em hdl} is not empty,
the next item is obtained from the buffer;
otherwise, when the input buffer is empty,
characters up to and including the next newline
character are read into the buffer.  No characters
are available until the newline character is
available.
\item[block-buffering] when the input buffer for {\em hdl} becomes empty,
the next block of data is read into this buffer.
\item[no-buffering] the next input item is read and returned.
\end{itemize}
For most implementations, physical files will normally be block-buffered 
and terminals will normally be line-buffered.

\begin{code}
-}
data BufferMode  =  NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
                    deriving (Eq, Ord, Read, Show)

hSetBuffering :: Handle -> BufferMode -> IO ()

hSetBuffering handle mode =
    case mode of
      (BlockBuffering (Just n)) 
        | n <= 0 -> fail (InvalidArgument "illegal buffer size")
      other ->
	  readHandle handle			    >>= \ htype ->
          if isMarked htype then
              writeHandle handle htype		    >>
              fail (UnsupportedOperation "can't set buffering for a dirty handle")
          else
              case htype of
	        ErrorHandle ioError ->
	            writeHandle handle htype	    >>
	            fail ioError
                ClosedHandle ->
		    writeHandle handle htype	    >>
		    fail (IllegalOperation "handle is closed")
                SemiClosedHandle _ _ ->
		    writeHandle handle htype	    >>
		    fail (IllegalOperation "handle is closed")
                other ->
                    _ccall_ setBuffering (filePtr other) bsize
						    `stThen` \ rc -> 
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

    hcon :: Handle__ -> (Addr -> (Maybe BufferMode) -> Bool -> Handle__)
    hcon (ReadHandle _ _ _) = ReadHandle
    hcon (WriteHandle _ _ _) = WriteHandle
    hcon (AppendHandle _ _ _) = AppendHandle
    hcon (ReadWriteHandle _ _ _) = ReadWriteHandle
{-
\end{code}

Computation $hSetBuffering hdl mode$ sets the mode of buffering for
handle {\em hdl} on subsequent reads and writes.

\begin{itemize}
\item
If {\em mode} is $LineBuffering$, line-buffering should be
enabled if possible.
\item
If {\em mode} is $BlockBuffering$ {\em size}, then block-buffering
should be enabled if possible.  The size of the buffer is {\em n} items
if {\em size} is $Just${\em n} and is otherwise implementation-dependent.
\item
If {\em mode} is $NoBuffering$, then buffering is disabled if possible.
\end{itemize}

If the buffer mode is changed from $BlockBuffering$ or $LineBuffering$
to $NoBuffering$, then any items in the output buffer are written to
the device, and any items in the input buffer are discarded.  The
default buffering mode when a handle is opened is
implementation-dependent and may depend on the object which is
attached to that handle.

\begin{code}
-}
hFlush :: Handle -> IO () 
hFlush handle = 
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
	  fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is closed")
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is closed")
      other ->
	  _ccall_ flushFile (filePtr other)	    `stThen` \ rc ->
	  writeHandle handle (markHandle htype)   >>
               if rc == 0 then 
		   return ()
               else
		    constructErrorAndFail "hFlush"
{-
\end{code}

Computation $flush hdl$ causes any items
buffered for output in handle {\em hdl} to be sent immediately to
the operating system.

\subsubsection[Seeking]{Repositioning Handles}

\begin{code}
-}
data HandlePosn = HandlePosn Handle Int

instance Eq HandlePosn{-partain-}

hGetPosn :: Handle -> IO HandlePosn
hGetPosn handle = 
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
	  fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is closed")
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is closed")
      other -> 
          _ccall_ getFilePosn (filePtr other)      `stThen` \ posn ->
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
	  fail (IllegalOperation "handle is closed")
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is closed")
      AppendHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is not seekable")
      other -> 
	  _ccall_ setFilePosn (filePtr other) posn `stThen` \ rc ->
	  writeHandle handle (markHandle htype)    >>
               if rc == 0 then 
		   return ()
               else
		   constructErrorAndFail "hSetPosn"
{-
\end{code}

Computation $hGetPosn hdl$ returns the current I/O
position of {\em hdl} as an abstract position.  Computation
$hSetPosn p$ sets the position of {\em hdl}
to a previously obtained position {\em p}.

\begin{code}
-}
data SeekMode    =  AbsoluteSeek | RelativeSeek | SeekFromEnd
                    deriving (Eq, Ord, Ix, Enum, Read, Show)

hSeek :: Handle -> SeekMode -> Integer -> IO () 
hSeek handle mode offset@(J# _ s# d#) = 
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
	  fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is closed")
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is closed")
      AppendHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is not seekable")
      other -> 
	  _ccall_ seekFile (filePtr other) whence (I# s#) (ByteArray (0,0) d#)
                                        	    `stThen` \ rc ->
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
{-
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

\subsubsection[Query]{Handle Properties}

\begin{code}
-}
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
	  fail (IllegalOperation "handle is closed")
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is closed")
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
	  fail (IllegalOperation "handle is closed")
      SemiClosedHandle _ _ ->
	  writeHandle handle htype	    >>
	  fail (IllegalOperation "handle is closed")
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
	  fail (IllegalOperation "handle is closed")
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is closed")
      other ->
          getBufferMode other			    `stThen` \ other ->
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
	  fail (IllegalOperation "handle is closed")
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is closed")
      other ->
	  getBufferMode other			    `stThen` \ other ->
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
	  fail (IllegalOperation "handle is closed")
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is closed")
      other ->
	  getBufferMode other			    `stThen` \ other ->
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
hGetBuffering hndl =
    readHandle hndl				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle hndl htype		    >>
          fail ioError
      ClosedHandle ->
	  writeHandle hndl htype		    >>
	  fail (IllegalOperation "handle is closed")
      SemiClosedHandle _ _ ->
	  writeHandle hndl htype		    >>
	  fail (IllegalOperation "handle is closed")
      other ->
	  getBufferMode other			    `stThen` \ other ->
          case bufferMode other of
            Just v ->
	        writeHandle hndl other	            >>
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
	  fail (IllegalOperation "handle is closed")
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
	  fail (IllegalOperation "handle is closed")
      AppendHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  return False
      other ->
	  _ccall_ seekFileP (filePtr other)   	    `stThen` \ rc ->
	  writeHandle handle htype		    >>
	  case rc of
            0 -> return False
            1 -> return True
            _ -> constructErrorAndFail "hIsSeekable"
{-
\end{code}

A number of operations return information about the properties of a
handle.  Each of these operations returns $True$ if the
handle has the specified property, and $False$
otherwise.

Computation $hIsBlockBuffered hdl$ returns $( False, Nothing )$ if
{\em hdl} is not block-buffered.  Otherwise it returns 
$( True, size )$, where {\em size} is $Nothing$ for default buffering, and 
$( Just n )$ for block-buffering of {\em n} bytes.
-}

-------------------------------------------------------------------
data IOError
  = AlreadyExists		String
  | HardwareFault		String
  | IllegalOperation		String
  | InappropriateType		String
  | Interrupted			String
  | InvalidArgument		String
  | NoSuchThing			String
  | OtherError			String
  | PermissionDenied		String
  | ProtocolError		String
  | ResourceBusy		String
  | ResourceExhausted		String
  | ResourceVanished		String
  | SystemError			String
  | TimeExpired			String
  | UnsatisfiedConstraints	String
  | UnsupportedOperation	String
  | UserError			String
  | EOF

instance Eq IOError where
    -- I don't know what the (pointless) idea is here,
    -- presumably just compare them by their tags (WDP)
    a == b = tag a == tag b
      where
	tag (AlreadyExists _)		= (1::Int)
	tag (HardwareFault _)		= 2
	tag (IllegalOperation _)	= 3
	tag (InappropriateType _)	= 4
	tag (Interrupted _)		= 5
	tag (InvalidArgument _)		= 6
	tag (NoSuchThing _)		= 7
	tag (OtherError _)		= 8
	tag (PermissionDenied _)	= 9
	tag (ProtocolError _)		= 10
	tag (ResourceBusy _)		= 11
	tag (ResourceExhausted _)	= 12
	tag (ResourceVanished _)	= 13
	tag (SystemError _)		= 14
	tag (TimeExpired _)		= 15
	tag (UnsatisfiedConstraints _)	= 16
	tag (UnsupportedOperation _)	= 17
	tag (UserError _)		= 18
	tag EOF				= 19

instance Show IOError where
    showsPrec p (AlreadyExists s)	= show2 "AlreadyExists: "	s
    showsPrec p (HardwareFault s)	= show2 "HardwareFault: "	s
    showsPrec p (IllegalOperation s)	= show2 "IllegalOperation: "	s
    showsPrec p (InappropriateType s)	= show2 "InappropriateType: "	s
    showsPrec p (Interrupted s)		= show2 "Interrupted: "		s
    showsPrec p (InvalidArgument s)	= show2 "InvalidArgument: "	s
    showsPrec p (NoSuchThing s)		= show2 "NoSuchThing: "		s
    showsPrec p (OtherError s)		= show2 "OtherError: "		s
    showsPrec p (PermissionDenied s)	= show2 "PermissionDenied: "	s
    showsPrec p (ProtocolError s)	= show2 "ProtocolError: "	s
    showsPrec p (ResourceBusy s)	= show2 "ResourceBusy: "	s
    showsPrec p (ResourceExhausted s)	= show2 "ResourceExhausted: "	s
    showsPrec p (ResourceVanished s)	= show2 "ResourceVanished: "	s
    showsPrec p (SystemError s)		= show2 "SystemError: "		s
    showsPrec p (TimeExpired s)		= show2 "TimeExpired: "		s
    showsPrec p (UnsatisfiedConstraints s) = show2 "UnsatisfiedConstraints: " s
    showsPrec p (UnsupportedOperation s)= show2 "UnsupportedOperation: " s
    showsPrec p (UserError s)		= showString s
    showsPrec p EOF			= showString "EOF"

show2 x y = showString x . showString y

{-

The @String@ part of an @IOError@ is platform-dependent.  However, to
provide a uniform mechanism for distinguishing among errors within
these broad categories, each platform-specific standard shall specify
the exact strings to be used for particular errors.  For errors not
explicitly mentioned in the standard, any descriptive string may be
used.

  SOF 4/96 - added argument to indicate function that flagged error
-}
constructErrorAndFail :: String -> IO a
constructError	      :: String -> PrimIO IOError

constructErrorAndFail call_site
  = stToIO (constructError call_site) >>= \ io_error ->
    fail io_error

constructError call_site
  = _casm_ ``%r = ghc_errtype;''    >>= \ (I# errtype#) ->
    _casm_ ``%r = ghc_errstr;''	    >>= \ str ->
    let
	msg = call_site ++ ':' : ' ' : GHCps.unpackPS (GHCps.packCString str)
    in
    return (case errtype# of
	ERR_ALREADYEXISTS#		-> AlreadyExists msg
	ERR_HARDWAREFAULT#		-> HardwareFault msg
	ERR_ILLEGALOPERATION#		-> IllegalOperation msg
	ERR_INAPPROPRIATETYPE#		-> InappropriateType msg
	ERR_INTERRUPTED#		-> Interrupted msg
	ERR_INVALIDARGUMENT#		-> InvalidArgument msg
	ERR_NOSUCHTHING#		-> NoSuchThing msg
	ERR_OTHERERROR#			-> OtherError msg
	ERR_PERMISSIONDENIED#		-> PermissionDenied msg
	ERR_PROTOCOLERROR#		-> ProtocolError msg
	ERR_RESOURCEBUSY#		-> ResourceBusy msg
	ERR_RESOURCEEXHAUSTED#		-> ResourceExhausted msg
	ERR_RESOURCEVANISHED#		-> ResourceVanished msg
	ERR_SYSTEMERROR#		-> SystemError msg
	ERR_TIMEEXPIRED#		-> TimeExpired msg
	ERR_UNSATISFIEDCONSTRAINTS#	-> UnsatisfiedConstraints msg
	ERR_UNSUPPORTEDOPERATION#	-> UnsupportedOperation msg
	ERR_EOF#			-> EOF
	_				-> OtherError "bad error construct"
    )
