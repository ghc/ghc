%
% (c) The GRASP/AQUA Project, Glasgow University, 1994
%
\section[PrelStdIO]{Haskell 1.3 Standard I/O}

This module defines Haskell {\em handles} and the operations which are
supported for them.

Haskell interfaces to the external world through an abstract {\em file
system}.  This file system is a collection of named {\em file system
objects}, which may be organised in {\em directories} (see
$LibDirectory$).  In some implementations, directories may themselves
be file system objects and could be entries in other directories.  For
simplicity, any non-directory file system object is termed a {\em
file}, although it could in fact be a communication channel, or any
other object recognised by the operating system.

File and directory names are values of type $String$, whose
precise meaning is operating system dependent.  Files can be opened,
yielding a handle which can then be used to operate on the contents
of that file.

\subsection[Handles]{Handles}

The standard defines operations to read/write finite sequences of
items from/to files, represented by values of type $Handle$.  Each
value of this type is a {\em handle}: a record used by the Haskell
run-time system to {\em manage} I/O with operating system objects.

A handle has at least the following properties:
\begin{itemize}
\item whether it manages input or output or both;
\item whether it is {\em open}, {\em closed} or {\em semi-closed};
\item the kind of object it manages;
\item if relevant, a current I/O position;
\item whether the object is seekable;
\item whether buffering is disabled, or enabled on a line or block basis;
\item a buffer (whose length may be zero).
\end{itemize}

A handle is {\em readable} if it manages only input or both input and
output; likewise, it is {\em writable} if it manages only output or
both input and output.  A handle is {\em open} when first allocated.
Once it is closed it can no longer be used for either input or output,
though an implementation cannot re-use its storage while references
remain to it.

\subsubsection[SemiClosed]{Semi-Closed Handles}

The operation $hGetContents$ puts a handle {\em hdl}
into an intermediate state, {\em semi-closed}.  In this state,
{\em hdl} is effectively closed, but items are read from
{\em hdl} on demand and accumulated in a special stream returned
by $hGetContents hdl$.

Any operation except for $hClose$ that fails because a handle is
closed, also fails if a handle is semi-closed.  A semi-closed handle
becomes closed:
\begin{itemize}
\item if $hClose$ is applied to it;
\item if an I/O error occurs when reading an item from the
file item from the stream;
\item or once the entire contents of the file has been read.
\end{itemize}

Once a semi-closed handle becomes closed, the contents of the
associated stream becomes fixed, and is the list of those items which
were successfully read from that handle. Any I/O errors encountered
when a handle is semi-closed are simply discarded.

\begin{code}
module PreludeStdIO (
    _Handle(..),
    Handle(..), 
    FilePath(..), 
    IOMode(..),
    BufferMode(..),
    HandlePosn(..),
    SeekMode(..),
    stdin13,
    stdout13,
    stderr13,
    openFile,
    hClose,
    hFileSize,
    hIsEOF,
    isEOF,
    hSetBuffering,
    hFlush,
    hGetPosn,
    hSetPosn,
    hSeek,
    hIsBlockBuffered,
    hIsLineBuffered,
    hIsNotBuffered,
    hIsOpen,
    hIsClosed,
    hIsReadable,
    hIsWritable,
    hIsSeekable,
    _filePtr,
    _bufferMode,
    _getBufferMode,
    _markHandle,
    Maybe(..)
  ) where

import Cls
import Core
import IChar
import IInt
import IList
import List		( (++) )
import PS		( _PackedString, _unpackPS )
import Prel		( otherwise, not, (.) )
import Text
import TyArray		-- instance _CCallable (_ByteArray a)

import PreludeIOError
import PreludeMonadicIO
import PreludePrimIO
import PreludeGlaST

---------------------------------
infixr 1 `my_then`

my_then	:: IO a -> (a -> PrimIO b) -> PrimIO b
{-# INLINE my_then   #-}

my_then m k = m `thenPrimIO` \ r -> k' r
  where
    k' (Right x)  = k x
    k' (Left err) = error "my_then"
---------------------------------

data Maybe a = Nothing | Just a {-partain-}deriving (Eq, Ord, Text)

data _Handle = _ErrorHandle IOError13
             | _ClosedHandle
             | _SemiClosedHandle _Addr (_Addr, Int)
             | _ReadHandle _Addr (Maybe BufferMode) Bool
             | _WriteHandle _Addr (Maybe BufferMode) Bool
	     | _AppendHandle _Addr (Maybe BufferMode) Bool
	     | _ReadWriteHandle _Addr (Maybe BufferMode) Bool
             | _SocketHandle _Addr Bool	    

type Handle = _MVar _Handle
type FilePath = String

_filePtr :: _Handle -> _Addr
_filePtr (_SemiClosedHandle fp buf) = fp
_filePtr (_ReadHandle fp _ _) = fp
_filePtr (_WriteHandle fp _ _) = fp
_filePtr (_AppendHandle fp _ _) = fp
_filePtr (_ReadWriteHandle fp _ _) = fp
_filePtr (_SocketHandle fp _) = fp

_bufferMode :: _Handle -> Maybe BufferMode
_bufferMode (_ReadHandle _ m _) = m
_bufferMode (_WriteHandle _ m _) = m
_bufferMode (_AppendHandle _ m _) = m
_bufferMode (_ReadWriteHandle _ m _) = m
_bufferMode (_SocketHandle _ _) = (Just NoBuffering)

_markHandle :: _Handle -> _Handle
_markHandle h@(_ReadHandle fp m b)
  | b = h
  | otherwise = _ReadHandle fp m True
_markHandle h@(_WriteHandle fp m b)
  | b = h
  | otherwise = _WriteHandle fp m True
_markHandle h@(_AppendHandle fp m b)
  | b = h
  | otherwise = _AppendHandle fp m True
_markHandle h@(_ReadWriteHandle fp m b)
  | b = h
  | otherwise = _ReadWriteHandle fp m True
_markHandle h@(_SocketHandle fp b)
  | b = h
  | otherwise = _SocketHandle fp True

\end{code}

\subsubsection[StandardHandles]{Standard Handles}

\begin{code}

stdin13, stdout13, stderr13 :: Handle

stdin13 = unsafePerformPrimIO (
    newEmptyMVar					`my_then` \ handle ->
    _ccall_ getLock (``stdin''::_Addr) 0		`thenPrimIO` \ rc ->
    (case rc of
       0 -> putMVar handle _ClosedHandle
       1 -> putMVar handle (_ReadHandle ``stdin'' Nothing False)
       _ -> _constructError				`thenPrimIO` \ ioError -> 
            putMVar handle (_ErrorHandle ioError)
    )							`seqPrimIO`
    returnPrimIO handle
  )

stdout13 = unsafePerformPrimIO (
    newEmptyMVar					`my_then` \ handle ->
    _ccall_ getLock (``stdout''::_Addr) 1		`thenPrimIO` \ rc ->
    (case rc of
       0 -> putMVar handle _ClosedHandle
       1 -> putMVar handle (_WriteHandle ``stdout'' Nothing False)
       _ -> _constructError				`thenPrimIO` \ ioError -> 
            putMVar handle (_ErrorHandle ioError)
    )							`seqPrimIO`
    returnPrimIO handle
  )

stderr13 = unsafePerformPrimIO (
    newEmptyMVar					`my_then` \ handle ->
    _ccall_ getLock (``stderr''::_Addr) 1		`thenPrimIO` \ rc ->
    (case rc of
       0 -> putMVar handle _ClosedHandle
       1 -> putMVar handle (_WriteHandle ``stderr'' (Just NoBuffering) False)	
       _ -> _constructError				`thenPrimIO` \ ioError -> 
            putMVar handle (_ErrorHandle ioError)
    )							`seqPrimIO`
    returnPrimIO handle
  )

\end{code}

Three handles are allocated during program initialisation.  The first
two manage input or output from the Haskell program's standard input
or output channel respectively.  The third manages output to the
standard error channel. These handles are initially open.

\subsubsection[OpeningClosing]{Opening and Closing Files}

\begin{code}

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

openFile :: FilePath -> IOMode -> IO Handle

openFile f m = 
    _ccall_ openFile f m'			    `thenPrimIO` \ ptr ->
    if ptr /= ``NULL'' then
        newEmptyMVar				    >>= \ handle ->
        putMVar handle (htype ptr Nothing False)    >>
        return handle
    else
	_constructError				    `thenPrimIO` \ ioError -> 
	let
	    improved_error -- a HACK, I guess
	      = case ioError of
		  AlreadyExists    msg -> AlreadyExists	   (msg ++ ": " ++ f)
		  NoSuchThing      msg -> NoSuchThing	   (msg ++ ": " ++ f)
		  PermissionDenied msg -> PermissionDenied (msg ++ ": " ++ f)
		  _		       -> ioError
	in
        failWith improved_error
  where
    m' = case m of 
           ReadMode      -> "r"
           WriteMode     -> "w"
           AppendMode    -> "a"
           ReadWriteMode -> "r+"
    htype = case m of 
              ReadMode      -> _ReadHandle
              WriteMode     -> _WriteHandle
              AppendMode    -> _AppendHandle
              ReadWriteMode -> _ReadWriteHandle

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
    takeMVar handle				    >>= \ htype ->
    putMVar handle _ClosedHandle		    >>
    case htype of 
      _ErrorHandle ioError ->
	  failWith ioError
      _ClosedHandle -> 
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle fp (buf,_) ->
          (if buf /= ``NULL'' then
	      _ccall_ free buf
           else			    
              returnPrimIO ())			    `thenPrimIO` \ () ->
          if fp /= ``NULL'' then
              _ccall_ closeFile fp		    `thenPrimIO` \ rc ->
              if rc == 0 then 
	          return ()
              else
	          _constructError		    `thenPrimIO` \ ioError ->
		  failWith ioError
          else			    
              return ()
      other -> 
          _ccall_ closeFile (_filePtr other)	    `thenPrimIO` \ rc ->
          if rc == 0 then 
	      return ()
          else
	      _constructError			    `thenPrimIO` \ ioError ->
	      failWith ioError
\end{code}

Computation $hClose hdl$ makes handle {\em hdl} closed.  Before the
computation finishes, any items buffered for output and not already
sent to the operating system are flushed as for $flush$.

\subsubsection[EOF]{Detecting the End of Input}

\begin{code}

hFileSize :: Handle -> IO Integer

hFileSize handle =
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
	  failWith ioError
      _ClosedHandle -> 
	  putMVar handle htype			    >>
          failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ -> 
	  putMVar handle htype			    >>
          failWith (IllegalOperation "handle is closed")
      _SocketHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "socket handles have no size")
      other ->
          -- HACK!  We build a unique MP_INT of the right shape to hold
          -- a single unsigned word, and we let the C routine change the data bits
          _casm_ ``%r = 1;''			    `thenPrimIO` \ (I# hack#) ->
          case int2Integer# hack# of
            result@(J# _ _ d#) ->
                _ccall_ fileSize (_filePtr other) (_ByteArray (error "fileSize") d#)
                                                    `thenPrimIO` \ rc ->
               putMVar handle htype		    `seqPrimIO`
               if rc == 0 then
		   return result
               else
		    _constructError		    `thenPrimIO` \ ioError ->
		    failWith ioError

\end{code}

For a handle {\em hdl} which attached to a physical file, $hFileSize
hdl$ returns the size of {\em hdl} in terms of the number of items
which can be read from {\em hdl}.

\begin{code}

hIsEOF :: Handle -> IO Bool
hIsEOF handle =
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
	  failWith ioError
      _ClosedHandle -> 
	  putMVar handle htype			    >>
          failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ -> 
	  putMVar handle htype			    >>
          failWith (IllegalOperation "handle is closed")
      _WriteHandle _ _ _ -> 
	  putMVar handle htype			    >>
          failWith (IllegalOperation "handle is not open for reading")
      _AppendHandle _ _ _ -> 
	  putMVar handle htype			    >>
          failWith (IllegalOperation "handle is not open for reading")
      other -> 
          _ccall_ fileEOF (_filePtr other)	    `thenPrimIO` \ rc ->
	  putMVar handle (_markHandle htype)	    >>
	  case rc of
            0 -> return False
            1 -> return True
            _ -> _constructError		    `thenPrimIO` \ ioError ->
		 failWith ioError

isEOF :: IO Bool
isEOF = hIsEOF stdin13

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

data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)

hSetBuffering :: Handle -> BufferMode -> IO ()

hSetBuffering handle mode =
    case mode of
      (BlockBuffering (Just n)) 
        | n <= 0 -> failWith (InvalidArgument "illegal buffer size")
      other ->
	  takeMVar handle			    >>= \ htype ->
          if isMarked htype then
              putMVar handle htype		    >>
              failWith (UnsupportedOperation "can't set buffering for a dirty handle")
          else
              case htype of
	        _ErrorHandle ioError ->
	            putMVar handle htype		    >>
	            failWith ioError
                _ClosedHandle ->
		    putMVar handle htype		    >>
		    failWith (IllegalOperation "handle is closed")
                _SemiClosedHandle _ _ ->
		    putMVar handle htype		    >>
		    failWith (IllegalOperation "handle is closed")
{-
		_SocketHandle _ _ ->
		    putMVar handle htype		    >>
		    failWith (IllegalOperation "buffering not supported for socket handles")
-}
                other ->
                    _ccall_ setBuffering (_filePtr other) bsize
							    `thenPrimIO` \ rc -> 
                    if rc == 0 then
                        putMVar handle ((hcon other) (_filePtr other) (Just mode) True)
							    >>
		        return ()
                    else
			putMVar handle htype		    >>
		        _constructError			    `thenPrimIO` \ ioError ->
		        failWith ioError
		
  where
    isMarked :: _Handle -> Bool
    isMarked (_ReadHandle fp m b) = b
    isMarked (_WriteHandle fp m b) = b
    isMarked (_AppendHandle fp m b) = b
    isMarked (_ReadWriteHandle fp m b) = b
    isMarked (_SocketHandle fp b) = b

    bsize :: Int
    bsize = case mode of
              NoBuffering -> 0
              LineBuffering -> -1
              BlockBuffering Nothing -> -2
              BlockBuffering (Just n) -> n

    hcon :: _Handle -> (_Addr -> (Maybe BufferMode) -> Bool -> _Handle)
    hcon (_ReadHandle _ _ _) = _ReadHandle
    hcon (_WriteHandle _ _ _) = _WriteHandle
    hcon (_AppendHandle _ _ _) = _AppendHandle
    hcon (_ReadWriteHandle _ _ _) = _ReadWriteHandle
    hcon (_SocketHandle _ _) = \ a _ v -> _SocketHandle a v

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

hFlush :: Handle -> IO () 

hFlush handle = 
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
	  failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SocketHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "flush not supported for socket handles")
      other ->
	  _ccall_ flushFile (_filePtr other)	    `thenPrimIO` \ rc ->
	  putMVar handle (_markHandle htype)	    >>
               if rc == 0 then 
		   return ()
               else
		    _constructError		    `thenPrimIO` \ ioError ->
		    failWith ioError

\end{code}

Computation $flush hdl$ causes any items
buffered for output in handle {\em hdl} to be sent immediately to
the operating system.

\subsubsection[Seeking]{Repositioning Handles}

\begin{code}

type HandlePosn = (Handle, Int)

hGetPosn :: Handle -> IO HandlePosn
hGetPosn handle = 
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
	  failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SocketHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "position not supported for socket handles")
      other -> 
          _ccall_ getFilePosn (_filePtr other)      `thenPrimIO` \ posn ->
          putMVar handle htype			    >>
          if posn /= -1 then
	      return (handle, posn)
          else
	      _constructError			    `thenPrimIO` \ ioError ->
	      failWith ioError

hSetPosn :: HandlePosn -> IO () 
hSetPosn (handle, posn) = 
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
	  failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _AppendHandle _ _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is not seekable")
      _SocketHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "seek not supported for socket handles")
      other -> 
	  _ccall_ setFilePosn (_filePtr other) posn `thenPrimIO` \ rc ->
	  putMVar handle (_markHandle htype)	    >>
               if rc == 0 then 
		   return ()
               else
		   _constructError		    `thenPrimIO` \ ioError ->
		   failWith ioError

\end{code}

Computation $hGetPosn hdl$ returns the current I/O
position of {\em hdl} as an abstract position.  Computation
$hSetPosn p$ sets the position of {\em hdl}
to a previously obtained position {\em p}.

\begin{code}

data SeekMode =  AbsoluteSeek | RelativeSeek | SeekFromEnd

hSeek :: Handle -> SeekMode -> Integer -> IO () 
hSeek handle mode offset@(J# _ s# d#) = 
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
	  failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _AppendHandle _ _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is not seekable")
      _SocketHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "seek not supported for socket handles")
      other -> 
	  _ccall_ seekFile (_filePtr other) whence (I# s#) (_ByteArray (0,0) d#)
                                        	    `thenPrimIO` \ rc ->
	  putMVar handle (_markHandle htype)	    >>
               if rc == 0 then 
		   return ()
               else
		    _constructError		    `thenPrimIO` \ ioError ->
		    failWith ioError
  where
    whence :: Int
    whence = case mode of
               AbsoluteSeek -> ``SEEK_SET''
               RelativeSeek -> ``SEEK_CUR''
               SeekFromEnd -> ``SEEK_END''

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

hIsOpen :: Handle -> IO Bool
hIsOpen handle = 
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
          failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  return False
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  return False
      other ->
	  putMVar handle htype			    >>
	  return True

hIsClosed :: Handle -> IO Bool
hIsClosed handle = 
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
          failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  return True
      other ->
	  putMVar handle htype			    >>
	  return False

hIsReadable :: Handle -> IO Bool
hIsReadable handle = 
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
          failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      other ->
	  putMVar handle htype			    >>
	  return (isReadable other)
  where
    isReadable (_ReadHandle _ _ _) = True
    isReadable (_ReadWriteHandle _ _ _) = True
    isReadable (_SocketHandle _ _) = True
    isReadable _ = False

hIsWritable :: Handle -> IO Bool
hIsWritable handle = 
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
          failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      other ->
	  putMVar handle htype			    >>
	  return (isWritable other)
  where
    isWritable (_AppendHandle _ _ _) = True
    isWritable (_WriteHandle _ _ _) = True
    isWritable (_ReadWriteHandle _ _ _) = True
    isWritable _ = False

_getBufferMode :: _Handle -> PrimIO _Handle
_getBufferMode htype =
    case _bufferMode htype of
      Just x -> returnPrimIO htype
      Nothing ->
	_ccall_ getBufferMode (_filePtr htype)	    `thenPrimIO` \ rc ->
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
	      _ReadHandle fp _ b -> _ReadHandle fp mode b
	      _WriteHandle fp _ b -> _WriteHandle fp mode b
	      _AppendHandle fp _ b -> _AppendHandle fp mode b
	      _ReadWriteHandle fp _ b -> _ReadWriteHandle fp mode b)

hIsBlockBuffered :: Handle -> IO (Bool,Maybe Int)
hIsBlockBuffered handle =
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
          failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      other ->
          _getBufferMode other			    `thenPrimIO` \ other ->
          case _bufferMode other of
            Just (BlockBuffering size) ->
	        putMVar handle other		    >>
                return (True, size)
            Just _ ->
	        putMVar handle other		    >>
                return (False, Nothing)
    	    Nothing -> 
		_constructError			    `thenPrimIO` \ ioError ->
		failWith ioError

hIsLineBuffered :: Handle -> IO Bool
hIsLineBuffered handle =
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
          failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      other ->
	  _getBufferMode other			    `thenPrimIO` \ other ->
          case _bufferMode other of
            Just LineBuffering ->
	        putMVar handle other		    >>
                return True
            Just _ ->
	        putMVar handle other		    >>
                return False
    	    Nothing -> 
		_constructError			    `thenPrimIO` \ ioError ->
		failWith ioError

hIsNotBuffered :: Handle -> IO Bool
hIsNotBuffered handle =
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
          failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      other ->
	  _getBufferMode other			    `thenPrimIO` \ other ->
          case _bufferMode other of
            Just NoBuffering ->
	        putMVar handle other		    >>
                return True
            Just _ ->
	        putMVar handle other		    >>
                return False
    	    Nothing -> 
		_constructError			    `thenPrimIO` \ ioError ->
		failWith ioError

hIsSeekable :: Handle -> IO Bool
hIsSeekable handle = 
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
          failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _AppendHandle _ _ _ ->
	  putMVar handle htype			    >>
	  return False
      _SocketHandle _ _ ->
	  putMVar handle htype			    >>
	  return False
      other ->
	  _ccall_ seekFileP (_filePtr other)   	    `thenPrimIO` \ rc ->
	  putMVar handle htype			    >>
	  case rc of
            0 -> return False
            1 -> return True
            _ -> _constructError		    `thenPrimIO` \ ioError ->
		 failWith ioError


\end{code}

A number of operations return information about the properties of a
handle.  Each of these operations returns $True$ if the
handle has the specified property, and $False$
otherwise.

Computation $hIsBlockBuffered hdl$ returns $( False, Nothing )$ if
{\em hdl} is not block-buffered.  Otherwise it returns 
$( True, size )$, where {\em size} is $Nothing$ for default buffering, and 
$( Just n )$ for block-buffering of {\em n} bytes.

