%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[IO]{Module @IO@}

Implementation of the standard Haskell IO interface, see
@http://haskell.org/onlinelibrary/io.html@ for the official
definition.

\begin{code}
{-# OPTIONS -fno-implicit-prelude -#include "cbits/stgio.h" #-}

module IO (
    Handle,		-- abstract, instance of: Eq, Show.
    HandlePosn(..),     -- abstract, instance of: Eq, Show.

    IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),
    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),

    stdin, stdout, stderr,   -- :: Handle

    openFile,		       -- :: FilePath -> IOMode -> IO Handle
    hClose,		       -- :: Handle -> IO ()
    hFileSize,		       -- :: Handle -> IO Integer
    hIsEOF,		       -- :: Handle -> IO Bool
    isEOF,		       -- :: IO Bool

    hSetBuffering,	       -- :: Handle -> BufferMode -> IO ()
    hGetBuffering,	       -- :: Handle -> IO BufferMode
    hFlush,		       -- :: Handle -> IO ()
    hGetPosn,		       -- :: Handle -> IO HandlePosn
    hSetPosn,		       -- :: Handle -> HandlePosn -> IO ()
    hSeek,		       -- :: Handle -> SeekMode -> Integer -> IO ()
    hWaitForInput,	       -- :: Handle -> Int -> IO Bool
    hReady,		       -- :: Handle -> IO Bool
    hGetChar,		       -- :: Handle -> IO Char
    hGetLine,		       -- :: Handle -> IO [Char]
    hLookAhead,		       -- :: Handle -> IO Char
    hGetContents,	       -- :: Handle -> IO [Char]
    hPutChar,		       -- :: Handle -> Char -> IO ()
    hPutStr,		       -- :: Handle -> [Char] -> IO ()
    hPutStrLn,		       -- :: Handle -> [Char] -> IO ()
    hPrint,		       -- :: Show a => Handle -> a -> IO ()
    hIsOpen, hIsClosed,        -- :: Handle -> IO Bool
    hIsReadable, hIsWritable,  -- :: Handle -> IO Bool
    hIsSeekable,               -- :: Handle -> IO Bool

    isAlreadyExistsError, isDoesNotExistError,  -- :: IOError -> Bool
    isAlreadyInUseError, isFullError, 
    isEOFError, isIllegalOperation, 
    isPermissionError, isUserError, 

    ioeGetErrorString,	       -- :: IOError -> String
    ioeGetHandle,	       -- :: IOError -> Maybe Handle
    ioeGetFileName,	       -- :: IOError -> Maybe FilePath

    try,		       -- :: IO a -> IO (Either IOError a)
    bracket,		       -- :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
    bracket_,		       -- :: IO a -> (a -> IO b) -> IO c -> IO c

    -- Non-standard extension (but will hopefully become standard with 1.5) is
    -- to export the Prelude io functions via IO (in addition to exporting them
    -- from the prelude...for now.) 
    putChar,		       -- :: Char   -> IO ()
    putStr,		       -- :: String -> IO () 
    putStrLn,		       -- :: String -> IO ()
    print,		       -- :: Show a => a -> IO ()
    getChar,		       -- :: IO Char
    getLine,		       -- :: IO String
    getContents,	       -- :: IO String
    interact,		       -- :: (String -> String) -> IO ()
    readFile,		       -- :: FilePath -> IO String
    writeFile,		       -- :: FilePath -> String -> IO ()
    appendFile,		       -- :: FilePath -> String -> IO ()
    readIO,		       -- :: Read a => String -> IO a
    readLn,		       -- :: Read a => IO a
    FilePath,		       -- :: String
    fail,		       -- :: IOError -> IO a
    catch,		       -- :: IO a    -> (IOError -> IO a) -> IO a
    userError,		       -- :: String  -> IOError

    IO,		-- non-standard, amazingly enough.
    IOError,    -- ditto

    -- extensions
    hPutBuf,
    hPutBufBA,
    slurpFile

  ) where

import PrelBase

import PrelIOBase
import PrelHandle		-- much of the real stuff is in here

import PrelRead         ( readParen, Read(..), reads, lex,
			  readIO 
			)
--import PrelNum		( toInteger )
import PrelBounded      ()  -- Bounded Int instance.
import PrelEither	( Either(..) )
import PrelAddr		( Addr(..), nullAddr )
import PrelArr		( ByteArray )
import PrelPack		( unpackNBytesAccST )

#ifndef __PARALLEL_HASKELL__
import PrelForeign  ( ForeignObj )
#endif

import Char		( ord, chr )

\end{code}

Standard instances for @Handle@:

\begin{code}
instance Eq IOError where
  (IOError h1 e1 loc1 str1) == (IOError h2 e2 loc2 str2) = 
    e1==e2 && str1==str2 && h1==h2 && loc1 == loc2

instance Eq Handle where
 (Handle h1) == (Handle h2) = h1 == h2

--Type declared in IOHandle, instance here because it depends on Eq.Handle
instance Eq HandlePosn where
    (HandlePosn h1 p1) == (HandlePosn h2 p2) = p1==p2 && h1==h2

-- Type declared in IOBase, instance here because it
-- depends on PrelRead.(Read Maybe) instance.
instance Read BufferMode where
    readsPrec p = 
      readParen False
	(\r ->	let lr = lex r
		in
		[(NoBuffering, rest)       | ("NoBuffering", rest) <- lr] ++
		[(LineBuffering,rest)      | ("LineBuffering",rest) <- lr] ++
		[(BlockBuffering mb,rest2) | ("BlockBuffering",rest1) <- lr,
		                             (mb, rest2) <- reads rest1])

\end{code}

%*********************************************************
%*							*
\subsection{Simple input operations}
%*							*
%*********************************************************

Computation @hReady hdl@ indicates whether at least
one item is available for input from handle {\em hdl}.

@hWaitForInput@ is the generalisation, wait for \tr{n} milliseconds
before deciding whether the Handle has run dry or not.

If @hWaitForInput@ finds anything in the Handle's buffer, it immediately returns.
If not, it tries to read from the underlying OS handle. Notice that
for buffered Handles connected to terminals this means waiting until a complete
line is available.

\begin{code}
hReady :: Handle -> IO Bool
hReady h = hWaitForInput h 0

hWaitForInput :: Handle -> Int -> IO Bool 
hWaitForInput handle msecs = do
    handle_  <- wantReadableHandle "hWaitForInput" handle
    rc       <- _ccall_ inputReady (haFO__ handle_) (msecs::Int)     -- ConcHask: SAFE, won't block
    writeHandle handle handle_
    case rc of
      0 -> return False
      1 -> return True
      _ -> constructErrorAndFail "hWaitForInput"
\end{code}

@hGetChar hdl@ reads the next character from handle @hdl@,
blocking until a character is available.

\begin{code}
hGetChar :: Handle -> IO Char
hGetChar handle = do
    handle_  <- wantReadableHandle "hGetChar" handle
    let fo = haFO__ handle_
    intc     <- mayBlock fo (_ccall_ fileGetc fo)  -- ConcHask: UNSAFE, may block
    writeHandle handle handle_
    if intc /= (-1)
     then return (chr intc)
     else constructErrorAndFail "hGetChar"

hGetLine :: Handle -> IO String
hGetLine h = do
  c <- hGetChar h
  if c == '\n' 
   then return "" 
   else do
     s <- hGetLine h
     return (c:s)

\end{code}

@hLookahead hdl@ returns the next character from handle @hdl@
without removing it from the input buffer, blocking until a
character is available.

\begin{code}
hLookAhead :: Handle -> IO Char
hLookAhead handle = do
    handle_ <- wantReadableHandle "hLookAhead" handle
    let fo = haFO__ handle_
    intc    <- mayBlock fo (_ccall_ fileLookAhead fo)  -- ConcHask: UNSAFE, may block
    writeHandle handle handle_
    if intc /= (-1)
     then return (chr intc)
     else constructErrorAndFail "hLookAhead"

\end{code}


%*********************************************************
%*							*
\subsection{Getting the entire contents of a handle}
%*							*
%*********************************************************

@hGetContents hdl@ returns the list of characters corresponding
to the unread portion of the channel or file managed by @hdl@,
which is made semi-closed.

\begin{code}
hGetContents :: Handle -> IO String
hGetContents handle = do
    handle_ <- wantReadableHandle "hGetContents" handle
      {- 
        To avoid introducing an extra layer of buffering here,
        we provide three lazy read methods, based on character,
        line, and block buffering.
      -}
    writeHandle handle (handle_{ haType__ = SemiClosedHandle })
    case (haBufferMode__ handle_) of
     LineBuffering    -> unsafeInterleaveIO (lazyReadLine handle (haFO__ handle_))
     BlockBuffering _ -> unsafeInterleaveIO (lazyReadBlock handle (haFO__ handle_))
     NoBuffering      -> unsafeInterleaveIO (lazyReadChar handle (haFO__ handle_))

\end{code}

Note that someone may close the semi-closed handle (or change its buffering), 
so each these lazy read functions are pulled on, they have to check whether
the handle has indeed been closed.

\begin{code}
#ifndef __PARALLEL_HASKELL__
lazyReadBlock :: Handle -> ForeignObj -> IO String
lazyReadLine  :: Handle -> ForeignObj -> IO String
lazyReadChar  :: Handle -> ForeignObj -> IO String
#else
lazyReadBlock :: Handle -> Addr -> IO String
lazyReadLine  :: Handle -> Addr -> IO String
lazyReadChar  :: Handle -> Addr -> IO String
#endif

lazyReadBlock handle fo = do
   buf   <- _ccall_ getBufStart fo (0::Int)
   bytes <- mayBlock fo (_ccall_ readBlock fo) -- ConcHask: UNSAFE, may block.
   case bytes of
     -3 -> -- buffering has been turned off, use lazyReadChar instead
           lazyReadChar handle fo
     -2 -> return ""
     -1 -> do -- an error occurred, close the handle
	  handle_ <- readHandle handle
          _ccall_ closeFile (haFO__ handle_) 0{-don't bother flushing-}  -- ConcHask: SAFE, won't block.
	  writeHandle handle (handle_ { haType__    = ClosedHandle,
					haFO__      = nullFile__ })
	  return ""
     _ -> do
      more <- unsafeInterleaveIO (lazyReadBlock handle fo)
      stToIO (unpackNBytesAccST buf bytes more)

lazyReadLine handle fo = do
     bytes <- mayBlock fo (_ccall_ readLine fo)   -- ConcHask: UNSAFE, may block.
     case bytes of
       -3 -> -- buffering has been turned off, use lazyReadChar instead
             lazyReadChar handle fo
       -2 -> return "" -- handle closed by someone else, stop reading.
       -1 -> do -- an error occurred, close the handle
	     handle_ <- readHandle handle
             _ccall_ closeFile (haFO__ handle_) 0{- don't bother flushing-}  -- ConcHask: SAFE, won't block
	     writeHandle handle (handle_ { haType__    = ClosedHandle,
					   haFO__      = nullFile__ })
	     return ""
       _ -> do
          more <- unsafeInterleaveIO (lazyReadLine handle fo)
          buf  <- _ccall_ getBufStart fo bytes  -- ConcHask: won't block
	  stToIO (unpackNBytesAccST buf bytes more)

lazyReadChar handle fo = do
    char <- mayBlock fo (_ccall_ readChar fo)   -- ConcHask: UNSAFE, may block.
    case char of
      -4 -> -- buffering is now block-buffered, use lazyReadBlock instead
	    lazyReadBlock handle fo
	    
      -3 -> -- buffering is now line-buffered, use lazyReadLine instead
	    lazyReadLine handle fo
      -2 -> return ""
      -1 -> do -- error, silently close handle.
         handle_ <- readHandle handle
         _ccall_ closeFile (haFO__ handle_) 0{-don't bother flusing-}  -- ConcHask: SAFE, won't block
	 writeHandle handle (handle_{ haType__  = ClosedHandle,
				      haFO__    = nullFile__ })
	 return ""
      _ -> do
	 more <- unsafeInterleaveIO (lazyReadChar handle fo)
         return (chr char : more)

\end{code}


%*********************************************************
%*							*
\subsection{Simple output functions}
%*							*
%*********************************************************

@hPutChar hdl ch@ writes the character @ch@ to the file
or channel managed by @hdl@.  Characters may be buffered if
buffering is enabled for @hdl@

\begin{code}
hPutChar :: Handle -> Char -> IO ()
hPutChar handle c = do
    handle_  <- wantWriteableHandle "hPutChar" handle
    let fo = haFO__ handle_
    rc       <- mayBlock fo (_ccall_ filePutc fo (ord c))   -- ConcHask: UNSAFE, may block.
    writeHandle handle handle_
    if rc == 0
     then return ()
     else constructErrorAndFail "hPutChar"

\end{code}

@hPutStr hdl s@ writes the string @s@ to the file or
channel managed by @hdl@, buffering the output if needs be.

\begin{code}
hPutStr :: Handle -> String -> IO ()
hPutStr handle str = do
    handle_ <- wantWriteableHandle "hPutStr" handle
    let fo = haFO__ handle_
    case haBufferMode__ handle_ of
       LineBuffering -> do
	    buf <- _ccall_ getWriteableBuf fo
	    pos <- _ccall_ getBufWPtr fo
	    bsz <- _ccall_ getBufSize fo
	    writeLines fo buf bsz pos str
       BlockBuffering _ -> do
	    buf <- _ccall_ getWriteableBuf fo
	    pos <- _ccall_ getBufWPtr fo
	    bsz <- _ccall_ getBufSize fo
            writeBlocks fo buf bsz pos str
       NoBuffering -> do
	    writeChars fo str
    writeHandle handle handle_

\end{code}

Going across the border between Haskell and C is relatively costly,
so for block writes we pack the character strings on the Haskell-side
before passing the external write routine a pointer to the buffer.

\begin{code}

#ifndef __PARALLEL_HASKELL__
writeLines :: ForeignObj -> Addr -> Int -> Int -> String -> IO ()
#else
writeLines :: Addr -> Addr -> Int -> Int -> String -> IO ()
#endif
writeLines obj buf bf@(I# bufLen) (I# initPos#) s =
  let
   write_char :: Addr -> Int# -> Char# -> IO ()
   write_char (A# buf) n# c# =
      IO $ \ s# ->
      case (writeCharOffAddr# buf n# c# s#) of s2# -> IOok s2# () 

   shoveString :: Int# -> [Char] -> IO ()
   shoveString n ls = 
     case ls of
      [] ->   
        if n ==# 0# then
	  _ccall_ setBufWPtr obj (0::Int)
        else do
	  {-
	    At the end of a buffer write, update the buffer position
	    in the underlying file object, so that if the handle
	    is subsequently dropped by the program, the whole
	    buffer will be properly flushed.

	    There's one case where this delayed up-date of the buffer
	    position can go wrong: if a thread is killed, it might be
	    in the middle of filling up a buffer, with the result that
	    the partial buffer update is lost upon finalisation. Not
	    that killing of threads is supported at the moment.

	  -}
	  _ccall_ setBufWPtr obj (I# n)

      ((C# x):xs) -> do
        write_char buf n x
          {- Flushing on buffer exhaustion or newlines (even if it isn't the last one) -}
	if n ==# bufLen || x `eqChar#` '\n'#
	 then do
	   rc <-  mayBlock obj (_ccall_ writeFileObject obj (I# (n +# 1#)))  -- ConcHask: UNSAFE, may block.
	   if rc == 0 
	    then shoveString 0# xs
	    else constructErrorAndFail "writeLines"
         else
	   shoveString (n +# 1#) xs
  in
  shoveString initPos# s

#ifndef __PARALLEL_HASKELL__
writeBlocks :: ForeignObj -> Addr -> Int -> Int -> String -> IO ()
#else
writeBlocks :: Addr -> Addr -> Int -> Int -> String -> IO ()
#endif
writeBlocks obj buf bf@(I# bufLen) (I# initPos#) s =
  let
   write_char :: Addr -> Int# -> Char# -> IO ()
   write_char (A# buf) n# c# =
      IO $ \ s# ->
      case (writeCharOffAddr# buf n# c# s#) of s2# -> IOok s2# () 

   shoveString :: Int# -> [Char] -> IO ()
   shoveString n ls = 
     case ls of
      [] ->   
        if n ==# 0# then
          _ccall_ setBufWPtr obj (0::Int)
        else do
	  {-
	    At the end of a buffer write, update the buffer position
	    in the underlying file object, so that if the handle
	    is subsequently dropped by the program, the whole
	    buffer will be properly flushed.

	    There's one case where this delayed up-date of the buffer
	    position can go wrong: if a thread is killed, it might be
	    in the middle of filling up a buffer, with the result that
	    the partial buffer update is lost upon finalisation. However,
	    by the time killThread is supported, Haskell finalisers are also
	    likely to be in, which means the 'IOFileObject' hack can go
	    alltogether.

	  -}
	  _ccall_ setBufWPtr obj (I# n)

      ((C# x):xs) -> do
        write_char buf n x
	if n ==# bufLen
	 then do
	   rc <-  mayBlock obj (_ccall_ writeFileObject obj (I# (n +# 1#)))   -- ConcHask: UNSAFE, may block.
	   if rc == 0 
	    then shoveString 0# xs
	    else constructErrorAndFail "writeChunks"
         else
	   shoveString (n +# 1#) xs
  in
  shoveString initPos# s

#ifndef __PARALLEL_HASKELL__
writeChars :: ForeignObj -> String -> IO ()
#else
writeChars :: Addr -> String -> IO ()
#endif
writeChars fo "" = return ()
writeChars fo (c:cs) = do
  rc <- mayBlock fo (_ccall_ filePutc fo (ord c))   -- ConcHask: UNSAFE, may block.
  if rc == 0 
   then writeChars fo cs
   else constructErrorAndFail "writeChars"

\end{code}

Computation @hPrint hdl t@ writes the string representation of {\em t}
given by the @shows@ function to the file or channel managed by {\em
hdl}.

[ Seem to have disappeared from the 1.4 interface  - SOF 2/97 ]

\begin{code}
hPrint :: Show a => Handle -> a -> IO ()
hPrint hdl = hPutStr hdl . show
\end{code}

Derived action @hPutStrLn hdl str@ writes the string \tr{str} to
the handle \tr{hdl}, adding a newline at the end.

\begin{code}
hPutStrLn :: Handle -> String -> IO ()
hPutStrLn hndl str = do
 hPutStr  hndl str
 hPutChar hndl '\n'

\end{code}


%*********************************************************
%*							*
\subsection{Try and bracket}
%*							*
%*********************************************************

The construct @try comp@ exposes errors which occur within a
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

%*********************************************************
%*							 *
\subsection{Standard IO}
%*							 *
%*********************************************************

The Prelude has from Day 1 provided a collection of common
IO functions. We define these here, but let the Prelude
export them.

\begin{code}
putChar         :: Char -> IO ()
putChar c       =  hPutChar stdout c

putStr          :: String -> IO ()
putStr s        =  hPutStr stdout s

putStrLn        :: String -> IO ()
putStrLn s      =  do putStr s
                      putChar '\n'

print           :: Show a => a -> IO ()
print x         =  putStrLn (show x)

getChar         :: IO Char
getChar         =  hGetChar stdin

getLine         :: IO String
getLine         =  hGetLine stdin
            
getContents     :: IO String
getContents     =  hGetContents stdin

interact        ::  (String -> String) -> IO ()
interact f      =   do s <- getContents
                       putStr (f s)

readFile        :: FilePath -> IO String
readFile name	=  openFile name ReadMode >>= hGetContents

writeFile       :: FilePath -> String -> IO ()
writeFile name str = do
    hdl <- openFile name WriteMode
    hPutStr hdl str
    hClose hdl

appendFile      :: FilePath -> String -> IO ()
appendFile name str = do
    hdl <- openFile name AppendMode
    hPutStr hdl str
    hClose hdl

readLn          :: Read a => IO a
readLn          =  do l <- getLine
                      r <- readIO l
                      return r
\end{code}
