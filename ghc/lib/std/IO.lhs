%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[IO]{Module @IO@}

Implementation of the standard Haskell IO interface, see
@http://haskell.org/onlinelibrary/io.html@ for the official
definition.

\begin{code}
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

  ) where

#ifndef __HUGS__
import PrelIOBase	-- Together these four Prelude modules define
import PrelHandle	-- all the stuff exported by IO for the GHC version
import PrelIO
import PrelException


-- The entire rest of this module is just Hugs

#else /* ifndef __HUGS__ */

import Ix(Ix)
import PrelPrim ( IORef
		, unsafePerformIO
		, prelCleanupAfterRunAction
		, copy_String_to_cstring
		, primIntToChar
		, primWriteCharOffAddr
		, nullAddr
		, newIORef
		, writeIORef
		, readIORef
		, nh_close
		, nh_errno
		, nh_stdin
		, nh_stdout
		, nh_stderr
		, nh_flush
		, nh_open
		, nh_free
		, nh_read
		, nh_write
		, nh_filesize
		, nh_iseof
		)
\end{code}


%*********************************************************
%*							*
\subsection{The HUGS version of IO
%*							*
%*********************************************************

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
        file = unsafePerformIO nh_stdin,
        mut  = unsafePerformIO (newIORef (Handle_Mut { state = HOpen })),
        mode = ReadMode
     }

stdout
   = Handle {
        name = "stdout",
        file = unsafePerformIO nh_stdout,
        mut  = unsafePerformIO (newIORef (Handle_Mut { state = HOpen })),
        mode = WriteMode
     }

stderr
   = Handle {
        name = "stderr",
        file = unsafePerformIO nh_stderr,
        mut  = unsafePerformIO (newIORef (Handle_Mut { state = HOpen })),
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
allHandles  = unsafePerformIO (newIORef [])

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
