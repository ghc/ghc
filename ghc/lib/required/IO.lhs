%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[IO]{Module @IO@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude -#include "cbits/stgio.h" #-}

module IO (
    Handle, HandlePosn,

    IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),
    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),

    stdin, stdout, stderr, 

    openFile, hClose, 
    hFileSize, hIsEOF, isEOF,
    hSetBuffering, hGetBuffering, hFlush, 
    hGetPosn, hSetPosn, hSeek, 
    hWaitForInput, hReady, hGetChar, hGetLine, hLookAhead, hGetContents, 
    hPutChar, hPutStr, hPutStrLn, hPrint,
    hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable,

    isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, 
    isFullError, isEOFError,
    isIllegalOperation, isPermissionError, isUserError, 
    ioeGetErrorString, 
    ioeGetHandle, ioeGetFileName,
    try, bracket, bracket_
  ) where

import Ix
import STBase
import IOBase
import ArrBase		( MutableByteArray(..), newCharArray )
import IOHandle		-- much of the real stuff is in here
import PackedString	( nilPS, packCBytesST, unpackPS )
import PrelBase
import GHC
import Foreign          ( ForeignObj, Addr, makeForeignObj, writeForeignObj )
\end{code}

%*********************************************************
%*							*
\subsection{Signatures}
%*							*
%*********************************************************

\begin{code}
--IOHandle:hClose                :: Handle -> IO () 
--IOHandle:hFileSize             :: Handle -> IO Integer
--IOHandle:hFlush                :: Handle -> IO () 
--IOHandle:hGetBuffering         :: Handle -> IO BufferMode
hGetChar              :: Handle -> IO Char
hGetContents          :: Handle -> IO String
--IOHandle:hGetPosn              :: Handle -> IO HandlePosn
--IOHandle:hIsClosed             :: Handle -> IO Bool
--IOHandle:hIsEOF                :: Handle -> IO Bool
--IOHandle:hIsOpen               :: Handle -> IO Bool
--IOHandle:hIsReadable           :: Handle -> IO Bool
--IOHandle:hIsSeekable           :: Handle -> IO Bool
--IOHandle:hIsWritable           :: Handle -> IO Bool
hLookAhead            :: Handle -> IO Char
hPrint                :: Show a => Handle -> a -> IO ()
hPutChar              :: Handle -> Char -> IO ()
hPutStr               :: Handle -> String -> IO ()
hPutStrLn             :: Handle -> String -> IO ()
hReady                :: Handle -> IO Bool 
hWaitForInput         :: Handle -> Int -> IO Bool

--IOHandle:hSeek                 :: Handle -> SeekMode -> Integer -> IO () 
--IOHandle:hSetBuffering         :: Handle -> BufferMode -> IO ()
--IOHandle:hSetPosn              :: HandlePosn -> IO () 
-- ioeGetFileName        :: IOError -> Maybe FilePath
-- ioeGetErrorString     :: IOError -> Maybe String
-- ioeGetHandle          :: IOError -> Maybe Handle
-- isAlreadyExistsError  :: IOError -> Bool
-- isAlreadyInUseError   :: IOError -> Bool
--IOHandle:isEOF                 :: IO Bool
-- isEOFError            :: IOError -> Bool
-- isFullError           :: IOError -> Bool
-- isIllegalOperation    :: IOError -> Bool
-- isPermissionError     :: IOError -> Bool
-- isUserError           :: IOError -> Maybe String
--IOHandle:openFile              :: FilePath -> IOMode -> IO Handle
--IOHandle:stdin, stdout, stderr :: Handle
\end{code}

Standard instances for @Handle@:

\begin{code}
instance Eq IOError where
  (IOError h1 e1 str1) == (IOError h2 e2 str2) = 
    e1==e2 && str1==str2 && h1==h2

instance Eq Handle where
 h1 == h2 =
  unsafePerformPrimIO (
    ioToPrimIO (readHandle h1)      >>= \ h1_ ->
    ioToPrimIO (writeHandle h1 h1_) >>
    ioToPrimIO (readHandle h2)      >>= \ h2_ ->
    ioToPrimIO (writeHandle h2 h2_) >>
    return (
     case (h1_,h2_) of
      (ErrorHandle (IOError h1 _ _), ErrorHandle (IOError h2 _ _)) -> h1 == h2
      (ClosedHandle, ClosedHandle) -> True
      (SemiClosedHandle v1 _, SemiClosedHandle v2 _) -> v1 == v2
      (ReadHandle v1 _ _ ,      ReadHandle v2 _ _)   -> v1 == v2
      (WriteHandle v1 _ _ ,     WriteHandle v2 _ _)  -> v1 == v2
      (AppendHandle v1 _ _ ,    AppendHandle v2 _ _) -> v1 == v2
      (ReadWriteHandle v1 _ _ , ReadWriteHandle v2 _ _) -> v1 == v2
      _ -> False))

instance Show Handle where {showsPrec p h = showString "<<Handle>>"}

\end{code}

%*********************************************************
%*							*
\subsection{Simple input operations}
%*							*
%*********************************************************

Computation @hReady hdl@ indicates whether at least
one item is available for input from handle {\em hdl}.

@hWaitForInput@ is the generalisation, wait for \tr{n} seconds
before deciding whether the Handle has run dry or not.

\begin{code}
--hReady :: Handle -> IO Bool
hReady h = hWaitForInput h 0

--hWaitForInput :: Handle -> Int -> IO Bool 
hWaitForInput handle nsecs = 
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
	  fail (IOError (Just handle) IllegalOperation "handle is not open for reading")
      WriteHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  fail (IOError (Just handle) IllegalOperation "handle is not open for reading")
      other -> 
	  _ccall_ inputReady (filePtr other) nsecs  `thenIO_Prim` \ rc ->
	  writeHandle handle (markHandle htype)     >>
          case rc of
            0 -> return False
            1 -> return True
            _ -> constructErrorAndFail "hWaitForInput"
\end{code}

Computation $hGetChar hdl$ reads the next character from handle 
{\em hdl}, blocking until a character is available.

\begin{code}
--hGetChar :: Handle -> IO Char

hGetChar handle = 
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
	  fail (IOError (Just handle) IllegalOperation "handle is not open for reading")
      WriteHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  fail (IOError (Just handle) IllegalOperation "handle is not open for reading")
      other -> 
	  _ccall_ fileGetc (filePtr other)  	    `thenIO_Prim` \ intc ->
	  writeHandle handle (markHandle htype)   >>
          if intc /= ``EOF'' then
              return (chr intc)
          else
              constructErrorAndFail "hGetChar"

hGetLine :: Handle -> IO String
hGetLine h = 
 hGetChar h >>= \ c ->
 if c == '\n' then 
    return "" 
 else 
    hGetLine h >>= \ s -> return (c:s)
\end{code}

Computation $hLookahead hdl$ returns the next character from handle
{\em hdl} without removing it from the input buffer, blocking until a
character is available.

\begin{code}
--hLookAhead :: Handle -> IO Char

hLookAhead handle = 
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
	  fail (IOError (Just handle) IllegalOperation "handle is not open for reading")
      WriteHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  fail (IOError (Just handle) IllegalOperation "handle is not open for reading")
      other -> 
	  _ccall_ fileLookAhead (filePtr other)    `thenIO_Prim` \ intc ->
	  writeHandle handle (markHandle htype)   >>
          if intc /= ``EOF'' then
              return (chr intc)
          else
              constructErrorAndFail "hLookAhead"
\end{code}


%*********************************************************
%*							*
\subsection{Getting the entire contents of a handle}
%*							*
%*********************************************************

Computation $hGetContents hdl$ returns the list of characters
corresponding to the unread portion of the channel or file managed by
{\em hdl}, which is made semi-closed.

\begin{code}
--hGetContents :: Handle -> IO String

hGetContents handle =
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
	  fail (IOError (Just handle) IllegalOperation "handle is not open for reading")
      WriteHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  fail (IOError (Just handle) IllegalOperation "handle is not open for reading")
      other -> 
	  {- 
             To avoid introducing an extra layer of buffering here,
             we provide three lazy read methods, based on character,
             line, and block buffering.
          -}
	  stToIO (getBufferMode other)	>>= \ other ->
          case (bufferMode other) of
            Just LineBuffering ->
		allocBuf Nothing		    >>= \ buf_info ->
	        writeHandle handle (SemiClosedHandle (filePtr other) buf_info)
                                          	    >>
                unsafeInterleavePrimIO (lazyReadLine handle)
						    `thenIO_Prim` \ contents ->
	        return contents

            Just (BlockBuffering size) ->
		allocBuf size			    >>= \ buf_info ->
	        writeHandle handle (SemiClosedHandle (filePtr other) buf_info)
                                          	    >>
                unsafeInterleavePrimIO (lazyReadBlock handle)
						    `thenIO_Prim` \ contents ->
	        return contents
            _ -> -- Nothing is treated pessimistically as NoBuffering
	        writeHandle handle (SemiClosedHandle (filePtr other) (``NULL'', 0))
                                          	    >>
                unsafeInterleavePrimIO (lazyReadChar handle)
						    `thenIO_Prim` \ contents ->
	        return contents
  where
    allocBuf :: Maybe Int -> IO (Addr, Int)
    allocBuf msize =
	_ccall_ malloc size		    	    `thenIO_Prim` \ buf ->
	if buf /= ``NULL'' then
	    return (buf, size)
	else
	    fail (IOError Nothing ResourceExhausted "not enough virtual memory")
      where
        size = 
	    case msize of
	      Just x -> x
	      Nothing -> ``BUFSIZ''
\end{code}

Note that someone may yank our handle out from under us, and then re-use
the same FILE * for something else.  Therefore, we have to re-examine the
handle every time through.

\begin{code}
lazyReadBlock :: Handle -> PrimIO String
lazyReadLine  :: Handle -> PrimIO String
lazyReadChar  :: Handle -> PrimIO String

lazyReadBlock handle =
    ioToST (readHandle handle)                  >>= \ htype ->
    case htype of 
      -- There cannae be an ErrorHandle here
      ClosedHandle ->
	  ioToST (writeHandle handle htype)	>>
	  returnPrimIO ""
      SemiClosedHandle fp (buf, size) ->
	  _ccall_ readBlock buf fp size		    >>= \ bytes ->
	  (if bytes <= 0
	  then return nilPS
	  else packCBytesST bytes buf)		    >>= \ some ->
          if bytes < 0 then
              _ccall_ free buf			    >>= \ () ->
              _ccall_ closeFile fp	            >>
#ifndef PAR
	      writeForeignObj fp ``NULL''           >>
	      ioToST (writeHandle handle (SemiClosedHandle fp (``NULL'', 0))) >>
#else
	      ioToST (writeHandle handle (SemiClosedHandle ``NULL'' (``NULL'', 0))) >>
#endif
	      returnPrimIO (unpackPS some)
	  else
	      ioToST (writeHandle handle htype)	    >>
              unsafeInterleavePrimIO (lazyReadBlock handle)
						    >>= \ more ->
	      returnPrimIO (unpackPS some ++ more)

lazyReadLine handle =
    ioToST (readHandle handle)                      >>= \ htype ->
    case htype of 
      -- There cannae be an ErrorHandle here
      ClosedHandle ->
	  ioToST (writeHandle handle htype) >>
	  returnPrimIO ""
      SemiClosedHandle fp (buf, size) ->
	  _ccall_ readLine buf fp size		    >>= \ bytes ->
	  (if bytes <= 0
	  then return nilPS
	  else packCBytesST bytes buf)		    >>= \ some ->
          if bytes < 0 then
              _ccall_ free buf			    >>= \ () ->
              _ccall_ closeFile fp	            >>
#ifndef PAR
	      writeForeignObj fp ``NULL''           >>
	      ioToST (writeHandle handle (SemiClosedHandle fp (``NULL'', 0))) >>
#else
	      ioToST (writeHandle handle (SemiClosedHandle ``NULL'' (``NULL'', 0))) >>
#endif
	      returnPrimIO (unpackPS some)
	  else
	      ioToST (writeHandle handle htype)	    >>
              unsafeInterleavePrimIO (lazyReadLine handle)
						    >>= \ more ->
	      returnPrimIO (unpackPS some ++ more)

lazyReadChar handle =
    ioToST (readHandle handle)                      >>= \ htype ->
    case htype of 
      -- There cannae be an ErrorHandle here
      ClosedHandle ->
	  ioToST (writeHandle handle htype)	    >>
	  returnPrimIO ""
      SemiClosedHandle fp buf_info ->
	  _ccall_ readChar fp			    >>= \ char ->
          if char == ``EOF'' then
              _ccall_ closeFile fp	            >>
#ifndef PAR
	      writeForeignObj fp ``NULL''           >>
	      ioToST (writeHandle handle (SemiClosedHandle fp (``NULL'', 0))) >>
#else
	      ioToST (writeHandle handle (SemiClosedHandle ``NULL'' (``NULL'', 0))) >>
#endif
	      returnPrimIO ""
	  else
	      ioToST (writeHandle handle htype)	    >>
              unsafeInterleavePrimIO (lazyReadChar handle)
						    >>= \ more ->
	      returnPrimIO (chr char : more)

\end{code}


%*********************************************************
%*							*
\subsection{Simple output functions}
%*							*
%*********************************************************

Computation $hPutChar hdl c$ writes the character {\em c} to the file
or channel managed by {\em hdl}.  Characters may be buffered if
buffering is enabled for {\em hdl}.

\begin{code}
--hPutChar :: Handle -> Char -> IO ()

hPutChar handle c =
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
      ReadHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  fail (IOError (Just handle) IllegalOperation "handle is not open for writing")
      other -> 
	  _ccall_ filePutc (filePtr other) (ord c) `thenIO_Prim` \ rc ->
	  writeHandle handle (markHandle htype)   >>
          if rc == 0 then
              return ()
          else
              constructErrorAndFail "hPutChar"
\end{code}

Computation $hPutStr hdl s$ writes the string {\em s} to the file or
channel managed by {\em hdl}.

\begin{code}
--hPutStr :: Handle -> String -> IO ()

hPutStr handle str = 
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
      ReadHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  fail (IOError (Just handle) IllegalOperation "handle is not open for writing")
      other -> 
          {-
           The code below is not correct for line-buffered terminal streams,
           as the output stream is not flushed when terminal input is requested
           again, just upon seeing a newline character. A temporary fix for the
           most common line-buffered output stream, stdout, is to assume the
           buffering it was given when created (no buffering). This is not
           as bad as it looks, since stdio buffering sits underneath this.

	   ToDo: fix me
	  -}
          getBufferMode other			    `thenIO_Prim` \ other ->
          (case bufferMode other of
            Just LineBuffering ->
	        writeChars (filePtr other) str
		--writeLines (filePtr other) str
            Just (BlockBuffering (Just size)) ->
	        writeBlocks (filePtr other) size str
            Just (BlockBuffering Nothing) ->
	        writeBlocks (filePtr other) ``BUFSIZ'' str
            _ -> -- Nothing is treated pessimistically as NoBuffering
	        writeChars (filePtr other) str
	  )    					    `thenIO_Prim` \ success ->
	  writeHandle handle (markHandle other) >>
          if success then
              return ()
          else
              constructErrorAndFail "hPutStr"
  where
#ifndef PAR
    writeLines :: ForeignObj -> String -> PrimIO Bool
#else
    writeLines :: Addr -> String -> PrimIO Bool
#endif
    writeLines = writeChunks ``BUFSIZ'' True 

#ifndef PAR
    writeBlocks :: ForeignObj -> Int -> String -> PrimIO Bool
#else
    writeBlocks :: Addr -> Int -> String -> PrimIO Bool
#endif
    writeBlocks fp size s = writeChunks size False fp s
 
    {-
      The breaking up of output into lines along \n boundaries
      works fine as long as there are newlines to split by.
      Avoid the splitting up into lines alltogether (doesn't work
      for overly long lines like the stuff that showsPrec instances
      normally return). Instead, we split them up into fixed size
      chunks before blasting them off to the Real World.

      Hacked to avoid multiple passes over the strings - unsightly, but
      a whole lot quicker. -- SOF 3/96
    -}

#ifndef PAR
    writeChunks :: Int -> Bool -> ForeignObj -> String -> PrimIO Bool
#else
    writeChunks :: Int -> Bool -> Addr -> String -> PrimIO Bool
#endif
    writeChunks (I# bufLen) chopOnNewLine fp s =
     newCharArray (0,I# bufLen) >>= \ arr@(MutableByteArray _ arr#) ->
     let
      write_char :: MutableByteArray# RealWorld -> Int# -> Char# -> PrimIO ()
      write_char arr# n x = ST $ \ (S# s#) ->
	  case (writeCharArray# arr# n x s#) of { s1# ->
	  ( (), S# s1# ) }

      shoveString :: Int# -> [Char] -> PrimIO Bool
      shoveString n ls = 
       case ls of
         [] ->   
	   if n ==# 0# then
	      returnPrimIO True
	   else
             _ccall_ writeFile arr fp (I# n) >>= \rc ->
             returnPrimIO (rc==0)

         ((C# x):xs) ->
	   write_char arr# n x	>>
	   
	   {- Flushing lines - should we bother? Yes, for line-buffered output. -}
	   if n ==# bufLen || (chopOnNewLine && (x `eqChar#` '\n'#)) then
	      _ccall_ writeFile arr fp (I# (n +# 1#)) >>= \ rc ->
	      if rc == 0 then
		 shoveString 0# xs
	       else
		 return False
	    else
	       shoveString (n +# 1#) xs
     in
     shoveString 0# s

#ifndef PAR
    writeChars :: ForeignObj -> String -> PrimIO Bool
#else
    writeChars :: Addr -> String -> PrimIO Bool
#endif
    writeChars fp "" = returnPrimIO True
    writeChars fp (c:cs) =
	_ccall_ filePutc fp (ord c) >>= \ rc ->
        if rc == 0 then
	    writeChars fp cs
	else
	    returnPrimIO False
\end{code}

Computation $hPrint hdl t$ writes the string representation of {\em t}
given by the $shows$ function to the file or channel managed by {\em
hdl}.

SOF 2/97: Seem to have disappeared in 1.4 libs.

\begin{code}
--hPrint :: Show a => Handle -> a -> IO ()
hPrint hdl = hPutStr hdl . show
\end{code}

Derived action @hPutStrLn hdl str@ writes the string \tr{str} to
the handle \tr{hdl}, adding a newline at the end.

\begin{code}
--hPutStrLn :: Handle -> String -> IO ()
hPutStrLn hndl str = do
 hPutStr  hndl str
 hPutChar hndl '\n'

\end{code}


%*********************************************************
%*							*
\subsection{Try and bracket}
%*							*
%*********************************************************

The construct $try comp$ exposes errors which occur within a
computation, and which are not fully handled.  It always succeeds.

\begin{code}
try            :: IO a -> IO (Either IOError a)
try f          =  catch (do r <- f
                            return (Right r))
                        (return . Left)

bracket        :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after m = do
        x  <- before
        rs <- try (m x)
        after x
        case rs of
           Right r -> return r
           Left  e -> fail e

-- variant of the above where middle computation doesn't want x
bracket_        :: IO a -> (a -> IO b) -> IO c -> IO c
bracket_ before after m = do
         x  <- before
         rs <- try m
         after x
         case rs of
            Right r -> return r
            Left  e -> fail e
\end{code}

