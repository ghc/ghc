
-----------------------------------------------------------------------------
-- Standard Library: IO operations, beyond those included in the prelude
--
-- WARNING: The names and semantics of functions defined in this module
-- may change as the details of the IO standard are clarified.
--
-- WARNING: extremely kludgey, incomplete and just plain wrong.
-----------------------------------------------------------------------------

module IO (
--  Handle, HandlePosn,
    Handle, 
--  IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),
    IOMode(ReadMode,WriteMode,AppendMode),
    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
    stdin, stdout, stderr, 
    openFile, hClose, 
--  hFileSize, hIsEOF, isEOF,
--  hSetBuffering, hGetBuffering, hFlush, 
    hFlush, 
    hGetPosn, hSetPosn, 
--  hSeek, hIsSeekable,
--  hReady, hGetChar, hLookAhead, hGetContents, 
    hGetChar, hGetLine, hGetContents, 
    hPutChar, hPutStr, hPutStrLn, hPrint,
    hIsOpen, hIsClosed, hIsReadable, hIsWritable, 
    isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, 
    isFullError, isEOFError,
    isIllegalOperation, isPermissionError, isUserError, 
    ioeGetErrorString, ioeGetHandle, ioeGetFileName,
    try, bracket, bracket_,

    -- ... and what the Prelude exports
    IO,
    FilePath, IOError, ioError, userError, catch,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, interact,
    readFile, writeFile, appendFile, readIO, readLn
    ) where

import Ix(Ix)

unimp :: String -> a
unimp s = error ("function not implemented: " ++ s)

type FILE_STAR = Int
type Ptr       = Int
nULL = 0 :: Int

data Handle 
   = Handle { name     :: FilePath,
              file     :: FILE_STAR,    -- C handle
              state    :: HState,       -- open/closed/semiclosed
              mode     :: IOMode,
              --seekable :: Bool,
              bmode    :: BufferMode,
              buff     :: Ptr,
              buffSize :: Int
            }

instance Eq Handle where
   h1 == h2   = file h1 == file h2

instance Show Handle where
   showsPrec _ h = showString ("<<handle " ++ name h ++ "=" ++ show (file h) ++ ">>")

data HandlePosn
   = HandlePosn 
     deriving (Eq, Show)


data IOMode      = ReadMode | WriteMode | AppendMode
                    deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)

data BufferMode  =  NoBuffering | LineBuffering 
                 |  BlockBuffering
                    deriving (Eq, Ord, Read, Show)

data SeekMode    =  AbsoluteSeek | RelativeSeek | SeekFromEnd
                    deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)

data HState = HOpen | HSemiClosed | HClosed
              deriving Eq

stdin  = Handle "stdin"  (primRunST nh_stdin)  HOpen ReadMode  NoBuffering   nULL 0
stdout = Handle "stdout" (primRunST nh_stdout) HOpen WriteMode LineBuffering nULL 0
stderr = Handle "stderr" (primRunST nh_stderr) HOpen WriteMode NoBuffering   nULL 0

openFile :: FilePath -> IOMode -> IO Handle
openFile f mode
   = copy_String_to_cstring f >>= \nameptr ->
     nh_open nameptr (mode2num mode) >>= \fh ->
     nh_free nameptr >>
     if   fh == nULL
     then (ioError.IOError) ("openFile: can't open " ++ f ++ " in " ++ show mode)
     else return (Handle f fh HOpen mode BlockBuffering nULL 0)
     where
        mode2num :: IOMode -> Int
        mode2num ReadMode   = 0
        mode2num WriteMode  = 1
        mode2num AppendMode = 2
        
hClose :: Handle -> IO ()
hClose h
   | not (state h == HOpen)
   = (ioError.IOError) ("hClose on non-open handle " ++ show h)
   | otherwise
   = nh_close (file h) >> 
     nh_errno >>= \err ->
     if   err == 0 
     then return ()
     else (ioError.IOError) ("hClose: error closing " ++ name h)

hFileSize             :: Handle -> IO Integer
hFileSize              = unimp "IO.hFileSize"
hIsEOF                :: Handle -> IO Bool
hIsEOF                 = unimp "IO.hIsEOF"
isEOF                 :: IO Bool
isEOF                  = hIsEOF stdin

hSetBuffering         :: Handle  -> BufferMode -> IO ()
hSetBuffering          = unimp "IO.hSetBuffering"
hGetBuffering         :: Handle  -> IO BufferMode
hGetBuffering          = unimp "IO.hGetBuffering"

hFlush :: Handle -> IO ()
hFlush h   
   = if   state h /= HOpen
     then (ioError.IOError) ("hFlush on closed/semiclosed file " ++ name h)
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
hReady h	       = hWaitForInput h 0

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

hGetContents :: Handle -> IO String
hGetContents h
   | not (state h == HOpen && mode h == ReadMode)
   = (ioError.IOError) ("hGetContents on invalid handle " ++ show h)
   | otherwise
   = read_all (file h)
     where
        read_all f 
           = unsafeInterleaveIO (
             nh_read f >>= \ci ->
             if   ci == -1
             then hClose h >> return []
             else read_all f >>= \rest -> 
                  return ((primIntToChar ci):rest)
             )

hPutStr :: Handle -> String -> IO ()
hPutStr h s
   | not (state h == HOpen && mode h /= ReadMode)
   = (ioError.IOError) ("hPutStr on invalid handle " ++ show h)
   | otherwise
   = write_all (file h) s
     where
        write_all f []
           = return ()
        write_all f (c:cs)
           = nh_write f (primCharToInt c) >>
             write_all f cs

hPutChar              :: Handle -> Char -> IO ()
hPutChar h c           = hPutStr h [c]

hPutStrLn             :: Handle -> String -> IO ()
hPutStrLn h s          = do { hPutStr h s; hPutChar h '\n' }

hPrint                :: Show a => Handle -> a -> IO ()
hPrint h               = hPutStrLn h . show

hIsOpen, hIsClosed, hIsReadable, hIsWritable :: Handle -> IO Bool
hIsOpen h              = return (state h == HOpen)
hIsClosed h            = return (state h == HClosed)
hIsReadable h          = return (mode h == ReadMode)
hIsWritable h          = return (mode h == WriteMode)

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
ioeGetErrorString = unimp "ioeGetErrorString"
ioeGetHandle      :: IOError -> Maybe Handle
ioeGetHandle      = unimp "ioeGetHandle"
ioeGetFileName    :: IOError -> Maybe FilePath
ioeGetFileName    = unimp "ioeGetFileName"

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

-----------------------------------------------------------------------------

