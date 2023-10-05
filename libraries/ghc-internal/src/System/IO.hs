{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, CApiFFI #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The standard IO library.
--
-----------------------------------------------------------------------------

module System.IO (
    -- * The IO monad

    IO,
    fixIO,

    -- * Files and handles

    FilePath,

    Handle,             -- abstract, instance of: Eq, Show.

    -- | GHC note: a 'Handle' will be automatically closed when the garbage
    -- collector detects that it has become unreferenced by the program.
    -- However, relying on this behaviour is not generally recommended:
    -- the garbage collector is unpredictable.  If possible, use
    -- an explicit 'hClose' to close 'Handle's when they are no longer
    -- required.  GHC does not currently attempt to free up file
    -- descriptors when they have run out, it is your responsibility to
    -- ensure that this doesn't happen.

    -- ** Standard handles

    -- | Three handles are allocated during program initialisation,
    -- and are initially open.

    stdin, stdout, stderr,

    -- * Opening and closing files

    -- ** Opening files

    withFile,
    openFile,
    IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),

    -- ** Closing files

    hClose,

    -- ** Special cases

    -- | These functions are also exported by the "Prelude".

    readFile,
    readFile',
    writeFile,
    appendFile,

    -- ** File locking

    -- $locking

    -- * Operations on handles

    -- ** Determining and changing the size of a file

    hFileSize,
    hSetFileSize,

    -- ** Detecting the end of input

    hIsEOF,
    isEOF,

    -- ** Buffering operations

    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    hSetBuffering,
    hGetBuffering,
    hFlush,

    -- ** Repositioning handles

    hGetPosn,
    hSetPosn,
    HandlePosn,                -- abstract, instance of: Eq, Show.

    hSeek,
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
    hTell,

    -- ** Handle properties

    hIsOpen, hIsClosed,
    hIsReadable, hIsWritable,
    hIsSeekable,

    -- ** Terminal operations (not portable: GHC only)

    hIsTerminalDevice,

    hSetEcho,
    hGetEcho,

    -- ** Showing handle state (not portable: GHC only)

    hShow,

    -- * Text input and output

    -- ** Text input

    hWaitForInput,
    hReady,
    hGetChar,
    hGetLine,
    hLookAhead,
    hGetContents,
    hGetContents',

    -- ** Text output

    hPutChar,
    hPutStr,
    hPutStrLn,
    hPrint,

    -- ** Special cases for standard input and output

    -- | These functions are also exported by the "Prelude".

    interact,
    putChar,
    putStr,
    putStrLn,
    print,
    getChar,
    getLine,
    getContents,
    getContents',
    readIO,
    readLn,

    -- * Binary input and output

    withBinaryFile,
    openBinaryFile,
    hSetBinaryMode,
    hPutBuf,
    hGetBuf,
    hGetBufSome,
    hPutBufNonBlocking,
    hGetBufNonBlocking,

    -- * Temporary files

    openTempFile,
    openBinaryTempFile,
    openTempFileWithDefaultPermissions,
    openBinaryTempFileWithDefaultPermissions,

    -- * Unicode encoding\/decoding

    -- | A text-mode 'Handle' has an associated 'TextEncoding', which
    -- is used to decode bytes into Unicode characters when reading,
    -- and encode Unicode characters into bytes when writing.
    --
    -- The default 'TextEncoding' is the same as the default encoding
    -- on your system, which is also available as 'localeEncoding'.
    -- (GHC note: on Windows, we currently do not support double-byte
    -- encodings; if the console\'s code page is unsupported, then
    -- 'localeEncoding' will be 'latin1'.)
    --
    -- Encoding and decoding errors are always detected and reported,
    -- except during lazy I/O ('hGetContents', 'getContents', and
    -- 'readFile'), where a decoding error merely results in
    -- termination of the character stream, as with other I/O errors.

    hSetEncoding,
    hGetEncoding,

    -- ** Unicode encodings
    TextEncoding,
    latin1,
    utf8, utf8_bom,
    utf16, utf16le, utf16be,
    utf32, utf32le, utf32be,
    localeEncoding,
    char8,
    mkTextEncoding,

    -- * Newline conversion

    -- | In Haskell, a newline is always represented by the character
    -- @\'\\n\'@.  However, in files and external character streams, a
    -- newline may be represented by another character sequence, such
    -- as @\'\\r\\n\'@.
    --
    -- A text-mode 'Handle' has an associated 'NewlineMode' that
    -- specifies how to translate newline characters.  The
    -- 'NewlineMode' specifies the input and output translation
    -- separately, so that for instance you can translate @\'\\r\\n\'@
    -- to @\'\\n\'@ on input, but leave newlines as @\'\\n\'@ on output.
    --
    -- The default 'NewlineMode' for a 'Handle' is
    -- 'nativeNewlineMode', which does no translation on Unix systems,
    -- but translates @\'\\r\\n\'@ to @\'\\n\'@ and back on Windows.
    --
    -- Binary-mode 'Handle's do no newline translation at all.
    --
    hSetNewlineMode,
    Newline(..), nativeNewline,
    NewlineMode(..),
    noNewlineTranslation, universalNewlineMode, nativeNewlineMode,
  ) where

import Control.Exception.Base

import Data.Bits
import Data.Maybe
import Foreign.C.Error
#if defined(mingw32_HOST_OS)
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils (with)
import Foreign.Storable
import GHC.IO.SubSystem
import GHC.IO.Windows.Handle (openFileAsTemp)
import GHC.IO.Handle.Windows (mkHandleFromHANDLE)
import GHC.IO.Device as IODevice
import GHC.Real (fromIntegral)
#endif
import Foreign.C.Types
import System.Posix.Internals
import System.Posix.Types

import GHC.Base
import GHC.List
#if !defined(mingw32_HOST_OS)
import GHC.IORef
#endif
import GHC.Num
import GHC.IO hiding ( bracket, onException )
import GHC.IO.IOMode
import qualified GHC.IO.FD as FD
import GHC.IO.Handle
import qualified GHC.IO.Handle.FD as POSIX
import GHC.IO.Handle.Text ( hGetBufSome, hPutStrLn )
import GHC.IO.Exception ( userError )
import GHC.IO.Encoding
import Text.Read
import GHC.IO.StdHandles
import GHC.Show
import GHC.MVar

-- -----------------------------------------------------------------------------
-- Standard IO

-- | Write a character to the standard output device
-- (same as 'hPutChar' 'stdout').

putChar         :: Char -> IO ()
putChar c       =  hPutChar stdout c

-- | Write a string to the standard output device
-- (same as 'hPutStr' 'stdout').

putStr          :: String -> IO ()
putStr s        =  hPutStr stdout s

-- | The same as 'putStr', but adds a newline character.

putStrLn        :: String -> IO ()
putStrLn s      =  hPutStrLn stdout s

-- | The 'print' function outputs a value of any printable type to the
-- standard output device.
-- Printable types are those that are instances of class 'Show'; 'print'
-- converts values to strings for output using the 'show' operation and
-- adds a newline.
--
-- For example, a program to print the first 20 integers and their
-- powers of 2 could be written as:
--
-- > main = print ([(n, 2^n) | n <- [0..19]])

print           :: Show a => a -> IO ()
print x         =  putStrLn (show x)

-- | Read a character from the standard input device
-- (same as 'hGetChar' 'stdin').

getChar         :: IO Char
getChar         =  hGetChar stdin

-- | Read a line from the standard input device
-- (same as 'hGetLine' 'stdin').

getLine         :: IO String
getLine         =  hGetLine stdin

-- | The 'getContents' operation returns all user input as a single string,
-- which is read lazily as it is needed
-- (same as 'hGetContents' 'stdin').

getContents     :: IO String
getContents     =  hGetContents stdin

-- | The 'getContents'' operation returns all user input as a single string,
-- which is fully read before being returned
-- (same as 'hGetContents'' 'stdin').
--
-- @since 4.15.0.0

getContents'    :: IO String
getContents'    =  hGetContents' stdin

-- | The 'interact' function takes a function of type @String->String@
-- as its argument.  The entire input from the standard input device is
-- passed to this function as its argument, and the resulting string is
-- output on the standard output device.

interact        ::  (String -> String) -> IO ()
interact f      =   do s <- getContents
                       putStr (f s)

-- | The 'readFile' function reads a file and
-- returns the contents of the file as a string.
-- The file is read lazily, on demand, as with 'getContents'.

readFile        :: FilePath -> IO String
readFile name   =  openFile name ReadMode >>= hGetContents

-- | The 'readFile'' function reads a file and
-- returns the contents of the file as a string.
-- The file is fully read before being returned, as with 'getContents''.
--
-- @since 4.15.0.0

readFile'       :: FilePath -> IO String
-- There's a bit of overkill here—both withFile and
-- hGetContents' will close the file in the end.
readFile' name  =  withFile name ReadMode hGetContents'

-- | The computation 'writeFile' @file str@ function writes the string @str@,
-- to the file @file@.
writeFile :: FilePath -> String -> IO ()
writeFile f txt = withFile f WriteMode (\ hdl -> hPutStr hdl txt)

-- | The computation 'appendFile' @file str@ function appends the string @str@,
-- to the file @file@.
--
-- Note that 'writeFile' and 'appendFile' write a literal string
-- to a file.  To write a value of any printable type, as with 'print',
-- use the 'show' function to convert the value to a string first.
--
-- > main = appendFile "squares" (show [(x,x*x) | x <- [0,0.1..2]])

appendFile      :: FilePath -> String -> IO ()
appendFile f txt = withFile f AppendMode (\ hdl -> hPutStr hdl txt)

-- | The 'readLn' function combines 'getLine' and 'readIO'.

readLn :: Read a => IO a
readLn = getLine >>= readIO

-- | The 'readIO' function is similar to 'read' except that it signals
-- parse failure to the 'IO' monad instead of terminating the program.

readIO          :: Read a => String -> IO a
readIO s        =  case (do { (x,t) <- reads s ;
                              ("","") <- lex t ;
                              return x }) of
                        [x]    -> return x
                        []     -> ioError (userError "Prelude.readIO: no parse")
                        _      -> ioError (userError "Prelude.readIO: ambiguous parse")

-- | The Unicode encoding of the current locale
--
-- This is the initial locale encoding: if it has been subsequently changed by
-- 'GHC.IO.Encoding.setLocaleEncoding' this value will not reflect that change.
localeEncoding :: TextEncoding
localeEncoding = initLocaleEncoding

-- | Computation 'hReady' @hdl@ indicates whether at least one item is
-- available for input from handle @hdl@.
--
-- This operation may fail with:
--
--  * 'System.IO.Error.isEOFError' if the end of file has been reached.

hReady          :: Handle -> IO Bool
hReady h        =  hWaitForInput h 0

-- | Computation 'hPrint' @hdl t@ writes the string representation of @t@
-- given by the 'shows' function to the file or channel managed by @hdl@
-- and appends a newline.
--
-- This operation may fail with:
--
--  * 'System.IO.Error.isFullError' if the device is full; or
--
--  * 'System.IO.Error.isPermissionError' if another system resource limit
--    would be exceeded.

hPrint          :: Show a => Handle -> a -> IO ()
hPrint hdl      =  hPutStrLn hdl . show


-- ---------------------------------------------------------------------------
-- fixIO

-- | The implementation of 'Control.Monad.Fix.mfix' for 'IO'. If the function
-- passed to 'fixIO' inspects its argument, the resulting action will throw
-- 'FixIOException'.
fixIO :: (a -> IO a) -> IO a
fixIO k = do
    m <- newEmptyMVar
    ans <- unsafeDupableInterleaveIO
             (readMVar m `catch` \BlockedIndefinitelyOnMVar ->
                                    throwIO FixIOException)
    result <- k ans
    putMVar m result
    return result

-- Note [Blackholing in fixIO]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We do our own explicit black holing here, because GHC's lazy
-- blackholing isn't enough.  In an infinite loop, GHC may run the IO
-- computation a few times before it notices the loop, which is wrong.
--
-- NOTE2: the explicit black-holing with an IORef ran into trouble
-- with multiple threads (see #5421), so now we use an MVar. We used
-- to use takeMVar with unsafeInterleaveIO. This, however, uses noDuplicate#,
-- which is not particularly cheap. Better to use readMVar, which can be
-- performed in multiple threads safely, and to use unsafeDupableInterleaveIO
-- to avoid the noDuplicate cost.
--
-- What we'd ideally want is probably an IVar, but we don't quite have those.
-- STM TVars look like an option at first, but I don't think they are:
-- we'd need to be able to write to the variable in an IO context, which can
-- only be done using 'atomically', and 'atomically' is not allowed within
-- unsafePerformIO. We can't know if someone will try to use the result
-- of fixIO with unsafePerformIO!
--
-- See also System.IO.Unsafe.unsafeFixIO.
--

-- | The function creates a temporary file in ReadWrite mode.
-- The created file isn\'t deleted automatically, so you need to delete it manually.
--
-- The file is created with permissions such that only the current
-- user can read\/write it.
--
-- With some exceptions (see below), the file will be created securely
-- in the sense that an attacker should not be able to cause
-- openTempFile to overwrite another file on the filesystem using your
-- credentials, by putting symbolic links (on Unix) in the place where
-- the temporary file is to be created.  On Unix the @O_CREAT@ and
-- @O_EXCL@ flags are used to prevent this attack, but note that
-- @O_EXCL@ is sometimes not supported on NFS filesystems, so if you
-- rely on this behaviour it is best to use local filesystems only.
--
openTempFile :: FilePath   -- ^ Directory in which to create the file
             -> String     -- ^ File name template. If the template is \"foo.ext\" then
                           -- the created file will be \"fooXXX.ext\" where XXX is some
                           -- random number. Note that this should not contain any path
                           -- separator characters. On Windows, the template prefix may
                           -- be truncated to 3 chars, e.g. \"foobar.ext\" will be
                           -- \"fooXXX.ext\".
             -> IO (FilePath, Handle)
openTempFile tmp_dir template
    = openTempFile' "openTempFile" tmp_dir template False 0o600

-- | Like 'openTempFile', but opens the file in binary mode. See 'openBinaryFile' for more comments.
openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFile tmp_dir template
    = openTempFile' "openBinaryTempFile" tmp_dir template True 0o600

-- | Like 'openTempFile', but uses the default file permissions
openTempFileWithDefaultPermissions :: FilePath -> String
                                   -> IO (FilePath, Handle)
openTempFileWithDefaultPermissions tmp_dir template
    = openTempFile' "openTempFileWithDefaultPermissions" tmp_dir template False 0o666

-- | Like 'openBinaryTempFile', but uses the default file permissions
openBinaryTempFileWithDefaultPermissions :: FilePath -> String
                                         -> IO (FilePath, Handle)
openBinaryTempFileWithDefaultPermissions tmp_dir template
    = openTempFile' "openBinaryTempFileWithDefaultPermissions" tmp_dir template True 0o666

openTempFile' :: String -> FilePath -> String -> Bool -> CMode
              -> IO (FilePath, Handle)
openTempFile' loc tmp_dir template binary mode
    | pathSeparator template
    = failIO $ "openTempFile': Template string must not contain path separator characters: "++template
    | otherwise = findTempName
  where
    -- We split off the last extension, so we can use .foo.ext files
    -- for temporary files (hidden on Unix OSes). Unfortunately we're
    -- below filepath in the hierarchy here.
    (prefix, suffix) =
       case break (== '.') $ reverse template of
         -- First case: template contains no '.'s. Just re-reverse it.
         (rev_suffix, "")       -> (reverse rev_suffix, "")
         -- Second case: template contains at least one '.'. Strip the
         -- dot from the prefix and prepend it to the suffix (if we don't
         -- do this, the unique number will get added after the '.' and
         -- thus be part of the extension, which is wrong.)
         (rev_suffix, '.':rest) -> (reverse rest, '.':reverse rev_suffix)
         -- Otherwise, something is wrong, because (break (== '.')) should
         -- always return a pair with either the empty string or a string
         -- beginning with '.' as the second component.
         _                      -> errorWithoutStackTrace "bug in System.IO.openTempFile"
#if defined(mingw32_HOST_OS)
    findTempName = findTempNamePosix <!> findTempNameWinIO

    findTempNameWinIO = do
      let label = if null prefix then "ghc" else prefix
      withCWString tmp_dir $ \c_tmp_dir ->
        withCWString label $ \c_template ->
          withCWString suffix $ \c_suffix ->
            with nullPtr $ \c_ptr -> do
              res <- c_createUUIDTempFileErrNo c_tmp_dir c_template c_suffix c_ptr
              if not res
                 then do errno <- getErrno
                         ioError (errnoToIOError loc errno Nothing (Just tmp_dir))
                 else do c_p <- peek c_ptr
                         filename <- peekCWString c_p
                         free c_p
                         let flags = fromIntegral mode .&. o_EXCL
                         handleResultsWinIO filename (flags == o_EXCL)

    findTempNamePosix = do
      let label = if null prefix then "ghc" else prefix
      withCWString tmp_dir $ \c_tmp_dir ->
        withCWString label $ \c_template ->
          withCWString suffix $ \c_suffix ->
            allocaBytes (sizeOf (undefined :: CWchar) * 260) $ \c_str -> do
            res <- c_getTempFileNameErrorNo c_tmp_dir c_template c_suffix 0
                                            c_str
            if not res
               then do errno <- getErrno
                       ioError (errnoToIOError loc errno Nothing (Just tmp_dir))
               else do filename <- peekCWString c_str
                       handleResultsPosix filename

    handleResultsPosix filename = do
      let oflags1 = rw_flags .|. o_EXCL
          binary_flags
              | binary    = o_BINARY
              | otherwise = 0
          oflags = oflags1 .|. binary_flags
      fd <- withFilePath filename $ \ f -> c_open f oflags mode
      case fd < 0 of
        True -> do errno <- getErrno
                   ioError (errnoToIOError loc errno Nothing (Just tmp_dir))
        False ->
          do (fD,fd_type) <- FD.mkFD fd ReadWriteMode Nothing{-no stat-}
                                     False{-is_socket-}
                                     True{-is_nonblock-}

             enc <- getLocaleEncoding
             h <- POSIX.mkHandleFromFD fD fd_type filename ReadWriteMode
                                 False{-set non-block-} (Just enc)

             return (filename, h)

    handleResultsWinIO filename excl = do
      (hwnd, hwnd_type) <- openFileAsTemp filename True excl
      mb_codec <- if binary then return Nothing else fmap Just getLocaleEncoding

      -- then use it to make a Handle
      h <- mkHandleFromHANDLE hwnd hwnd_type filename ReadWriteMode mb_codec
                `onException` IODevice.close hwnd
      return (filename, h)

foreign import ccall "getTempFileNameErrorNo" c_getTempFileNameErrorNo
  :: CWString -> CWString -> CWString -> CUInt -> Ptr CWchar -> IO Bool

foreign import ccall "__createUUIDTempFileErrNo" c_createUUIDTempFileErrNo
  :: CWString -> CWString -> CWString -> Ptr CWString -> IO Bool

pathSeparator :: String -> Bool
pathSeparator template = any (\x-> x == '/' || x == '\\') template

output_flags = std_flags
#else /* else mingw32_HOST_OS */
    findTempName = do
      rs <- rand_string
      let filename = prefix ++ rs ++ suffix
          filepath = tmp_dir `combine` filename
      r <- openNewFile filepath binary mode
      case r of
        FileExists -> findTempName
        OpenNewError errno -> ioError (errnoToIOError loc errno Nothing (Just tmp_dir))
        NewFileCreated fd -> do
          (fD,fd_type) <- FD.mkFD fd ReadWriteMode Nothing{-no stat-}
                               False{-is_socket-}
                               True{-is_nonblock-}

          enc <- getLocaleEncoding
          h <- POSIX.mkHandleFromFD fD fd_type filepath ReadWriteMode False{-set non-block-} (Just enc)

          return (filepath, h)

      where
        -- XXX bits copied from System.FilePath, since that's not available here
        combine a b
                  | null b = a
                  | null a = b
                  | pathSeparator [last a] = a ++ b
                  | otherwise = a ++ [pathSeparatorChar] ++ b

tempCounter :: IORef Int
tempCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE tempCounter #-}

-- build large digit-alike number
rand_string :: IO String
rand_string = do
  r1 <- c_getpid
  (r2, _) <- atomicModifyIORef'_ tempCounter (+1)
  return $ show r1 ++ "-" ++ show r2

data OpenNewFileResult
  = NewFileCreated CInt
  | FileExists
  | OpenNewError Errno

openNewFile :: FilePath -> Bool -> CMode -> IO OpenNewFileResult
openNewFile filepath binary mode = do
  let oflags1 = rw_flags .|. o_EXCL

      binary_flags
        | binary    = o_BINARY
        | otherwise = 0

      oflags = oflags1 .|. binary_flags
  fd <- withFilePath filepath $ \ f ->
          c_open f oflags mode
  if fd < 0
    then do
      errno <- getErrno
      case errno of
        _ | errno == eEXIST -> return FileExists
        _ -> return (OpenNewError errno)
    else return (NewFileCreated fd)

-- XXX Should use filepath library
pathSeparatorChar :: Char
pathSeparatorChar = '/'

pathSeparator :: String -> Bool
pathSeparator template = pathSeparatorChar `elem` template

output_flags = std_flags    .|. o_CREAT
#endif /* mingw32_HOST_OS */

-- XXX Copied from GHC.Handle
std_flags, output_flags, rw_flags :: CInt
std_flags    = o_NONBLOCK   .|. o_NOCTTY
rw_flags     = output_flags .|. o_RDWR

-- $locking
-- Implementations should enforce as far as possible, at least locally to the
-- Haskell process, multiple-reader single-writer locking on files.
-- That is, /there may either be many handles on the same file which manage input, or just one handle on the file which manages output/.  If any
-- open or semi-closed handle is managing a file for output, no new
-- handle can be allocated for that file.  If any open or semi-closed
-- handle is managing a file for input, new handles can only be allocated
-- if they do not manage output.  Whether two files are the same is
-- implementation-dependent, but they should normally be the same if they
-- have the same absolute path name and neither has been renamed, for
-- example.
--
-- /Warning/: the 'readFile' operation holds a semi-closed handle on
-- the file until the entire contents of the file have been consumed.
-- It follows that an attempt to write to a file (using 'writeFile', for
-- example) that was earlier opened by 'readFile' will usually result in
-- failure with 'System.IO.Error.isAlreadyInUseError'.
