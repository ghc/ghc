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
    IO,
    FilePath,		       -- :: String
    IOError,
    ioError,		       -- :: IOError -> IO a
    userError,		       -- :: String  -> IOError
    catch,		       -- :: IO a    -> (IOError -> IO a) -> IO a
    interact,		       -- :: (String -> String) -> IO ()

    putChar,		       -- :: Char   -> IO ()
    putStr,		       -- :: String -> IO () 
    putStrLn,		       -- :: String -> IO ()
    print,		       -- :: Show a => a -> IO ()
    getChar,		       -- :: IO Char
    getLine,		       -- :: IO String
    getContents,	       -- :: IO String
    readFile,		       -- :: FilePath -> IO String
    writeFile,		       -- :: FilePath -> String -> IO ()
    appendFile,		       -- :: FilePath -> String -> IO ()
    readIO,		       -- :: Read a => String -> IO a
    readLn,		       -- :: Read a => IO a

#ifndef __HUGS__
    -- extensions
    hPutBuf,
    hPutBufBA,
#endif
    slurpFile

  ) where

#ifdef __HUGS__
import Ix(Ix)
#else
--import PrelST
import PrelBase

import PrelIOBase
import PrelHandle		-- much of the real stuff is in here

import PrelRead         ( readParen, Read(..), reads, lex,
			  readIO 
			)
import PrelShow
import PrelMaybe	( Either(..), Maybe(..) )
import PrelAddr		( Addr(..), nullAddr )
import PrelArr		( ByteArray )
import PrelPack		( unpackNBytesAccST )
import PrelException    ( ioError, catch )

#ifndef __PARALLEL_HASKELL__
import PrelForeign  ( ForeignObj )
#endif

import Char		( ord, chr )

#endif /* ndef __HUGS__ */
\end{code}

#ifndef __HUGS__
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
hWaitForInput handle msecs =
    wantReadableHandle "hWaitForInput" handle $ \ handle_ -> do
    rc       <- inputReady (haFO__ handle_) (msecs::Int)     -- ConcHask: SAFE, won't block
    case (rc::Int) of
      0 -> return False
      1 -> return True
      _ -> constructErrorAndFail "hWaitForInput"
\end{code}

@hGetChar hdl@ reads the next character from handle @hdl@,
blocking until a character is available.

\begin{code}
hGetChar :: Handle -> IO Char
hGetChar handle = 
    wantReadableHandle "hGetChar" handle $ \ handle_ -> do
    let fo = haFO__ handle_
    intc     <- mayBlock fo (fileGetc fo)  -- ConcHask: UNSAFE, may block
    if intc /= ((-1)::Int)
     then return (chr intc)
     else constructErrorAndFail "hGetChar"

{-
  If EOF is reached before EOL is encountered, ignore the
  EOF and return the partial line. Next attempt at calling
  hGetLine on the handle will yield an EOF IO exception though.
-}
hGetLine :: Handle -> IO String
hGetLine h = do
  c <- hGetChar h
  if c == '\n' then
     return ""
   else do
    l <- getRest
    return (c:l)
 where
  getRest = do
    c <- 
      catch 
        (hGetChar h)
        (\ err -> do
          if isEOFError err then
	     return '\n'
	   else
	     ioError err)
    if c == '\n' then
       return ""
     else do
       s <- getRest
       return (c:s)

\end{code}

@hLookahead hdl@ returns the next character from handle @hdl@
without removing it from the input buffer, blocking until a
character is available.

\begin{code}
hLookAhead :: Handle -> IO Char
hLookAhead handle =
    wantReadableHandle "hLookAhead" handle $ \ handle_ -> do
    let fo = haFO__ handle_
    intc    <- mayBlock fo (fileLookAhead fo)  -- ConcHask: UNSAFE, may block
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
hGetContents handle = 
	-- can't use wantReadableHandle here, because we want to side effect
	-- the handle.
    withHandle handle $ \ handle_ -> do
    case haType__ handle_ of 
      ErrorHandle theError -> ioError theError
      ClosedHandle 	   -> ioe_closedHandle "hGetContents" handle
      SemiClosedHandle 	   -> ioe_closedHandle "hGetContents" handle
      AppendHandle 	   -> ioError not_readable_error
      WriteHandle 	   -> ioError not_readable_error
      _ -> do
    	  {- 
    	    To avoid introducing an extra layer of buffering here,
    	    we provide three lazy read methods, based on character,
    	    line, and block buffering.
    	  -}
   	let handle_' = handle_{ haType__ = SemiClosedHandle }
    	case (haBufferMode__ handle_) of
    	 LineBuffering    -> do
    	    str <- unsafeInterleaveIO (lazyReadLine handle (haFO__ handle_))
    	    return (handle_', str)
    	 BlockBuffering _ -> do
    	    str <- unsafeInterleaveIO (lazyReadBlock handle (haFO__ handle_))
    	    return (handle_', str)
    	 NoBuffering      -> do
    	    str <- unsafeInterleaveIO (lazyReadChar handle (haFO__ handle_))
    	    return (handle_', str)
  where
   not_readable_error = 
	   IOError (Just handle) IllegalOperation "hGetContents"
		   ("handle is not open for reading")
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
   buf   <- getBufStart fo 0
   bytes <- mayBlock fo (readBlock fo) -- ConcHask: UNSAFE, may block.
   case (bytes::Int) of
     -3 -> -- buffering has been turned off, use lazyReadChar instead
           lazyReadChar handle fo
     -2 -> return ""
     -1 -> -- an error occurred, close the handle
	  withHandle handle $ \ handle_ -> do
          closeFile (haFO__ handle_) 0{-don't bother flushing-}  -- ConcHask: SAFE, won't block.
	  return (handle_ { haType__    = ClosedHandle,
			    haFO__      = nullFile__ }, 
		  "")
     _ -> do
      more <- unsafeInterleaveIO (lazyReadBlock handle fo)
      stToIO (unpackNBytesAccST buf bytes more)

lazyReadLine handle fo = do
     bytes <- mayBlock fo (readLine fo)   -- ConcHask: UNSAFE, may block.
     case (bytes::Int) of
       -3 -> -- buffering has been turned off, use lazyReadChar instead
             lazyReadChar handle fo
       -2 -> return "" -- handle closed by someone else, stop reading.
       -1 -> -- an error occurred, close the handle
  	     withHandle handle $ \ handle_ -> do
             closeFile (haFO__ handle_) 0{- don't bother flushing-}  -- ConcHask: SAFE, won't block
	     return (handle_ { haType__    = ClosedHandle,
			       haFO__      = nullFile__ },
		     "")
       _ -> do
          more <- unsafeInterleaveIO (lazyReadLine handle fo)
          buf  <- getBufStart fo bytes  -- ConcHask: won't block
	  stToIO (unpackNBytesAccST buf bytes more)

lazyReadChar handle fo = do
    char <- mayBlock fo (readChar fo)   -- ConcHask: UNSAFE, may block.
    case (char::Int) of
      -4 -> -- buffering is now block-buffered, use lazyReadBlock instead
	    lazyReadBlock handle fo
	    
      -3 -> -- buffering is now line-buffered, use lazyReadLine instead
	    lazyReadLine handle fo
      -2 -> return ""
      -1 -> -- error, silently close handle.
 	 withHandle handle $ \ handle_ -> do
         closeFile (haFO__ handle_) 0{-don't bother flusing-}  -- ConcHask: SAFE, won't block
	 return (handle_{ haType__  = ClosedHandle,
			  haFO__    = nullFile__ },
		 "")
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
hPutChar handle c = 
    wantWriteableHandle "hPutChar" handle $ \ handle_  -> do
    let fo = haFO__ handle_
    flushConnectedBuf fo
    rc       <- mayBlock fo (filePutc fo c)   -- ConcHask: UNSAFE, may block.
    if rc == 0
     then return ()
     else constructErrorAndFail "hPutChar"

\end{code}

@hPutStr hdl s@ writes the string @s@ to the file or
channel managed by @hdl@, buffering the output if needs be.

\begin{code}
hPutStr :: Handle -> String -> IO ()
hPutStr handle str = 
    wantWriteableHandle "hPutStr" handle $ \ handle_ -> do
    let fo = haFO__ handle_
    flushConnectedBuf fo
    case haBufferMode__ handle_ of
       LineBuffering -> do
	    buf <- getWriteableBuf fo
	    pos <- getBufWPtr fo
	    bsz <- getBufSize fo
	    writeLines fo buf bsz pos str
       BlockBuffering _ -> do
	    buf <- getWriteableBuf fo
	    pos <- getBufWPtr fo
	    bsz <- getBufSize fo
            writeBlocks fo buf bsz pos str
       NoBuffering -> do
	    writeChars fo str
\end{code}

Going across the border between Haskell and C is relatively costly,
so for block writes we pack the character strings on the Haskell-side
before passing the external write routine a pointer to the buffer.

\begin{code}
#ifdef __HUGS__

#ifdef __CONCURRENT_HASKELL__
/* See comment in shoveString below for explanation */
#warning delayed update of buffer disnae work with killThread
#endif

#ifndef __PARALLEL_HASKELL__
writeLines :: ForeignObj -> Addr -> Int -> Int -> String -> IO ()
#else
writeLines :: Addr -> Addr -> Int -> Int -> String -> IO ()
#endif
writeLines obj buf bufLen initPos s =
  let
   shoveString :: Int -> [Char] -> IO ()
   shoveString n ls = 
     case ls of
      [] ->   
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
	  setBufWPtr obj n

      (x:xs) -> do
        primWriteCharOffAddr buf n x
          {- Flushing on buffer exhaustion or newlines (even if it isn't the last one) -}
	if n == bufLen || x == '\n'
	 then do
	   rc <-  mayBlock obj (writeFileObject obj (n + 1))  -- ConcHask: UNSAFE, may block.
	   if rc == 0 
	    then shoveString 0 xs
	    else constructErrorAndFail "writeLines"
         else
	   shoveString (n + 1) xs
  in
  shoveString initPos s
#else /* ndef __HUGS__ */
#ifndef __PARALLEL_HASKELL__
writeLines :: ForeignObj -> Addr -> Int -> Int -> String -> IO ()
#else
writeLines :: Addr -> Addr -> Int -> Int -> String -> IO ()
#endif
writeLines obj buf (I# bufLen) (I# initPos#) s =
  let
   write_char :: Addr -> Int# -> Char# -> IO ()
   write_char (A# buf#) n# c# =
      IO $ \ s# ->
      case (writeCharOffAddr# buf# n# c# s#) of s2# -> (# s2#, () #)

   shoveString :: Int# -> [Char] -> IO ()
   shoveString n ls = 
     case ls of
      [] ->   
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
	  setBufWPtr obj (I# n)

      ((C# x):xs) -> do
        write_char buf n x
          {- Flushing on buffer exhaustion or newlines (even if it isn't the last one) -}
	if n ==# bufLen || x `eqChar#` '\n'#
	 then do
	   rc <-  mayBlock obj (writeFileObject obj (I# (n +# 1#)))  -- ConcHask: UNSAFE, may block.
	   if rc == 0 
	    then shoveString 0# xs
	    else constructErrorAndFail "writeLines"
         else
	   shoveString (n +# 1#) xs
  in
  shoveString initPos# s
#endif /* ndef __HUGS__ */

#ifdef __HUGS__
#ifndef __PARALLEL_HASKELL__
writeBlocks :: ForeignObj -> Addr -> Int -> Int -> String -> IO ()
#else
writeBlocks :: Addr -> Addr -> Int -> Int -> String -> IO ()
#endif
writeBlocks obj buf bufLen initPos s =
  let
   shoveString :: Int -> [Char] -> IO ()
   shoveString n ls = 
     case ls of
      [] ->   
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
	  setBufWPtr obj n

      (x:xs) -> do
        primWriteCharOffAddr buf n x
	if n == bufLen
	 then do
	   rc <-  mayBlock obj (writeFileObject obj (n + 1))   -- ConcHask: UNSAFE, may block.
	   if rc == 0 
            then shoveString 0 xs
	    else constructErrorAndFail "writeChunks"
         else
	   shoveString (n + 1) xs
  in
  shoveString initPos s
#else /* ndef __HUGS__ */
#ifndef __PARALLEL_HASKELL__
writeBlocks :: ForeignObj -> Addr -> Int -> Int -> String -> IO ()
#else
writeBlocks :: Addr -> Addr -> Int -> Int -> String -> IO ()
#endif
writeBlocks obj buf (I# bufLen) (I# initPos#) s =
  let
   write_char :: Addr -> Int# -> Char# -> IO ()
   write_char (A# buf#) n# c# =
      IO $ \ s# ->
      case (writeCharOffAddr# buf# n# c# s#) of s2# -> (# s2#, () #)

   shoveString :: Int# -> [Char] -> IO ()
   shoveString n ls = 
     case ls of
      [] ->   
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
	  setBufWPtr obj (I# n)

      ((C# x):xs) -> do
        write_char buf n x
	if n ==# bufLen
	 then do
	   rc <-  mayBlock obj (writeFileObject obj (I# (n +# 1#)))   -- ConcHask: UNSAFE, may block.
	   if rc == 0 
	    then shoveString 0# xs
	    else constructErrorAndFail "writeChunks"
         else
	   shoveString (n +# 1#) xs
  in
  shoveString initPos# s
#endif /* ndef __HUGS__ */

#ifndef __PARALLEL_HASKELL__
writeChars :: ForeignObj -> String -> IO ()
#else
writeChars :: Addr -> String -> IO ()
#endif
writeChars _fo ""    = return ()
writeChars fo (c:cs) = do
  rc <- mayBlock fo (filePutc fo c)   -- ConcHask: UNSAFE, may block.
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
hPrint hdl = hPutStrLn hdl . show
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
           Left  e -> ioError e

-- variant of the above where middle computation doesn't want x
bracket_        :: IO a -> (a -> IO b) -> IO c -> IO c
bracket_ before after m = do
         x  <- before
         rs <- try m
         after x
         case rs of
            Right r -> return r
            Left  e -> ioError e
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

#else /* __HUGS__ */

\begin{code}
import Ix(Ix)
import Monad(when)

unimp :: String -> a
unimp s = error ("IO library: function not implemented: " ++ s)

type FILE_STAR = Addr
type Ptr       = Addr
nULL           = nullAddr

data Handle 
   = Handle { name     :: FilePath,
              file     :: FILE_STAR,         -- C handle
              mut      :: IORef Handle_Mut,  -- open/closed/semiclosed
              mode     :: IOMode,
              seekable :: Bool
            }

data Handle_Mut
   = Handle_Mut { state :: HState 
                }
     deriving Show

set_state :: Handle -> HState -> IO ()
set_state hdl new_state
   = writeIORef (mut hdl) (Handle_Mut { state = new_state })
get_state :: Handle -> IO HState
get_state hdl
   = readIORef (mut hdl) >>= \m -> return (state m)

mkErr :: Handle -> String -> IO a
mkErr h msg
   = do mut <- readIORef (mut h)
        when (state mut /= HClosed) 
             (nh_close (file h) >> set_state h HClosed)
        dummy <- nh_errno
        ioError (IOError msg)

stdin
   = Handle {
        name = "stdin",
        file = primRunST nh_stdin,
        mut  = primRunST (newIORef (Handle_Mut { state = HOpen })),
        mode = ReadMode
     }

stdout
   = Handle {
        name = "stdout",
        file = primRunST nh_stdout,
        mut  = primRunST (newIORef (Handle_Mut { state = HOpen })),
        mode = WriteMode
     }

stderr
   = Handle {
        name = "stderr",
        file = primRunST nh_stderr,
        mut  = primRunST (newIORef (Handle_Mut { state = HOpen })),
        mode = WriteMode
     }


instance Eq Handle where
   h1 == h2   = file h1 == file h2

instance Show Handle where
   showsPrec _ h = showString ("`" ++ name h ++ "'")

data HandlePosn
   = HandlePosn 
     deriving (Eq, Show)


data IOMode      = ReadMode | WriteMode | AppendMode | ReadWriteMode
                    deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)

data BufferMode  =  NoBuffering | LineBuffering 
                 |  BlockBuffering (Maybe Int)
                    deriving (Eq, Ord, Read, Show)

data SeekMode    =  AbsoluteSeek | RelativeSeek | SeekFromEnd
                    deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)

data HState = HOpen | HSemiClosed | HClosed
              deriving (Show, Eq)


-- A global variable holding a list of all open handles.
-- Each handle is present as many times as it has been opened.
-- Any given file is allowed to have _either_ one writeable handle
-- or many readable handles in this list.  The list is used to
-- enforce single-writer multiple reader semantics.  It also 
-- provides a list of handles for System.exitWith to flush and
-- close.  In order not to have to put all this stuff in the
-- Prelude, System.exitWith merely runs prelExitWithAction,
-- which is originally Nothing, but which we set to Just ...
-- once handles appear in the list.

allHandles :: IORef [Handle]
allHandles  = primRunST (newIORef [])

elemWriterHandles :: FilePath -> IO Bool
elemAllHandles    :: FilePath -> IO Bool
addHandle         :: Handle -> IO ()
delHandle         :: Handle -> IO ()
cleanupHandles    :: IO ()

cleanupHandles
   = do hdls <- readIORef allHandles
        mapM_ cleanupHandle hdls
     where
        cleanupHandle h
           | mode h == ReadMode
           = nh_close (file h) 
             >> nh_errno >>= \_ -> return ()
           | otherwise
           = nh_flush (file h) >> nh_close (file h) 
             >> nh_errno >>= \_ -> return ()

elemWriterHandles fname
   = do hdls <- readIORef allHandles
        let hdls_w = filter ((/= ReadMode).mode) hdls
        return (fname `elem` (map name hdls_w))

elemAllHandles fname
   = do hdls <- readIORef allHandles
        return (fname `elem` (map name hdls))

addHandle hdl
   = do cleanup_action <- readIORef prelCleanupAfterRunAction
        case cleanup_action of
           Nothing 
              -> writeIORef prelCleanupAfterRunAction (Just cleanupHandles)
           Just xx
              -> return ()
        hdls <- readIORef allHandles
        writeIORef allHandles (hdl : hdls)

delHandle hdl
   = do hdls <- readIORef allHandles
        let hdls' = takeWhile (/= hdl) hdls 
                    ++ drop 1 (dropWhile (/= hdl) hdls)
        writeIORef allHandles hdls'



openFile :: FilePath -> IOMode -> IO Handle
openFile f mode

   | null f
   =  (ioError.IOError) "openFile: empty file name"

   | mode == ReadMode
   = do not_ok <- elemWriterHandles f
        if    not_ok 
         then (ioError.IOError) 
                 ("openFile: `" ++ f ++ "' in " ++ show mode 
                  ++ ": is already open for writing")
         else openFile_main f mode

   | mode /= ReadMode
   = do not_ok <- elemAllHandles f
        if    not_ok 
         then (ioError.IOError) 
                 ("openFile: `" ++ f ++ "' in " ++ show mode 
                  ++ ": is already open for reading or writing")
         else openFile_main f mode

   | otherwise
   = openFile_main f mode

openFile_main f mode
   = copy_String_to_cstring f >>= \nameptr ->
     nh_open nameptr (mode2num mode) >>= \fh ->
     nh_free nameptr >>
     if   fh == nULL
     then (ioError.IOError)
             ("openFile: can't open <<" ++ f ++ ">> in " ++ show mode)
     else do r   <- newIORef (Handle_Mut { state = HOpen })
             let hdl = Handle { name = f, file = fh, 
                                mut  = r, mode = mode }
             addHandle hdl
             return hdl
     where
        mode2num :: IOMode -> Int
        mode2num ReadMode   = 0
        mode2num WriteMode  = 1
        mode2num AppendMode = 2
        mode2num ReadWriteMode
           = error
                ("openFile <<" ++ f ++ ">>: ReadWriteMode not supported")

hClose :: Handle -> IO ()
hClose h
   = do mut <- readIORef (mut h)
        putStrLn ( "hClose: state is " ++ show mut)
        if    state mut == HClosed
         then mkErr h
                 ("hClose on closed handle " ++ show h)
         else 
         do set_state h HClosed
            delHandle h
            nh_close (file h)
            err <- nh_errno
            if    err == 0 
             then return ()
             else mkErr h
                     ("hClose: error closing " ++ name h)

hGetContents :: Handle -> IO String
hGetContents h
   | mode h /= ReadMode
   = mkErr h ("hGetContents on non-ReadMode handle " ++ show h)
   | otherwise 
   = do mut <- readIORef (mut h)
        if    state mut /= HOpen
         then mkErr h
                 ("hGetContents on closed/semiclosed handle " ++ show h)
         else
         do set_state h HSemiClosed
            read_all (file h)
            where
               read_all f 
                  = nh_read f >>= \ci ->
                    if   ci == -1
                    then return []
                    else read_all f >>= \rest -> 
                         return ((primIntToChar ci):rest)

hPutStr :: Handle -> String -> IO ()
hPutStr h s
   | mode h == ReadMode
   = mkErr h ("hPutStr on ReadMode handle " ++ show h)
   | otherwise
   = do mut <- readIORef (mut h)
        if    state mut /= HOpen
         then mkErr h
                 ("hPutStr on closed/semiclosed handle " ++ show h)
         else write_all (file h) s
              where
                 write_all f []
                    = return ()
                 write_all f (c:cs)
                    = nh_write f c >> write_all f cs

hFileSize :: Handle -> IO Integer
hFileSize h
   = do sz <- nh_filesize (file h)
        er <- nh_errno
        if    er == 0
         then return (fromIntegral sz)
         else mkErr h ("hFileSize on " ++ show h)

hIsEOF :: Handle -> IO Bool
hIsEOF h
   = do iseof <- nh_iseof (file h)
        er    <- nh_errno
        if    er == 0
         then return (iseof /= 0)
         else mkErr h ("hIsEOF on " ++ show h)

isEOF :: IO Bool
isEOF = hIsEOF stdin

hSetBuffering         :: Handle  -> BufferMode -> IO ()
hSetBuffering          = unimp "IO.hSetBuffering"
hGetBuffering         :: Handle  -> IO BufferMode
hGetBuffering          = unimp "IO.hGetBuffering"

hFlush :: Handle -> IO ()
hFlush h
   = do mut <- readIORef (mut h)
        if    state mut /= HOpen
         then mkErr h
                 ("hFlush on closed/semiclosed file " ++ name h)
         else nh_flush (file h)

hGetPosn              :: Handle -> IO HandlePosn
hGetPosn               = unimp "IO.hGetPosn"
hSetPosn              :: HandlePosn -> IO ()
hSetPosn               = unimp "IO.hSetPosn"
hSeek                 :: Handle -> SeekMode -> Integer -> IO () 
hSeek                  = unimp "IO.hSeek"
hWaitForInput	      :: Handle -> Int -> IO Bool
hWaitForInput          = unimp "hWaitForInput"
hReady                :: Handle -> IO Bool 
hReady h	       = unimp "hReady" -- hWaitForInput h 0

hGetChar    :: Handle -> IO Char
hGetChar h
   = nh_read (file h) >>= \ci ->
     return (primIntToChar ci)

hGetLine              :: Handle -> IO String
hGetLine h             = do c <- hGetChar h
                            if c=='\n' then return ""
                              else do cs <- hGetLine h
                                      return (c:cs)

hLookAhead            :: Handle -> IO Char
hLookAhead             = unimp "IO.hLookAhead"


hPutChar              :: Handle -> Char -> IO ()
hPutChar h c           = hPutStr h [c]

hPutStrLn             :: Handle -> String -> IO ()
hPutStrLn h s          = do { hPutStr h s; hPutChar h '\n' }

hPrint                :: Show a => Handle -> a -> IO ()
hPrint h               = hPutStrLn h . show

hIsOpen, hIsClosed, hIsReadable, hIsWritable :: Handle -> IO Bool
hIsOpen h              = do { s <- get_state h; return (s == HOpen) }
hIsClosed h            = do { s <- get_state h; return (s == HClosed) }
hIsReadable h          = return (mode h == ReadMode)
hIsWritable h          = return (mode h `elem` [WriteMode, AppendMode])

hIsSeekable           :: Handle -> IO Bool
hIsSeekable            = unimp "IO.hIsSeekable"

isIllegalOperation, 
	  isAlreadyExistsError, 
	  isDoesNotExistError, 
          isAlreadyInUseError,   
	  isFullError,     
          isEOFError, 
	  isPermissionError,
          isUserError        :: IOError -> Bool

isIllegalOperation    = unimp "IO.isIllegalOperation"
isAlreadyExistsError  = unimp "IO.isAlreadyExistsError"
isDoesNotExistError   = unimp "IO.isDoesNotExistError"
isAlreadyInUseError   = unimp "IO.isAlreadyInUseError"
isFullError           = unimp "IO.isFullError"
isEOFError            = unimp "IO.isEOFError"
isPermissionError     = unimp "IO.isPermissionError"
isUserError           = unimp "IO.isUserError"


ioeGetErrorString :: IOError -> String
ioeGetErrorString = unimp "IO.ioeGetErrorString"
ioeGetHandle      :: IOError -> Maybe Handle
ioeGetHandle      = unimp "IO.ioeGetHandle"
ioeGetFileName    :: IOError -> Maybe FilePath
ioeGetFileName    = unimp "IO.ioeGetFileName"

try       :: IO a -> IO (Either IOError a)
try p      = catch (p >>= (return . Right)) (return . Left)

bracket        :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after m = do
        x  <- before
        rs <- try (m x)
        after x
        case rs of
           Right r -> return r
           Left  e -> ioError e

-- variant of the above where middle computation doesn't want x
bracket_        :: IO a -> (a -> IO b) -> IO c -> IO c
bracket_ before after m = do
         x  <- before
         rs <- try m
         after x
         case rs of
            Right r -> return r
            Left  e -> ioError e

-- TODO: Hugs/slurpFile
slurpFile = unimp "IO.slurpFile"
\end{code}

#endif /* #ifndef __HUGS__ */
