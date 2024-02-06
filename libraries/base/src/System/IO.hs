{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  System.IO
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The standard IO API.
--

module System.IO
    (-- * Examples
     -- $stdio_examples

     -- *  The IO monad
     IO,
     fixIO,
     -- *  Files and handles
     FilePath,
     Handle,
     -- |  GHC note: a 'Handle' will be automatically closed when the garbage
     -- collector detects that it has become unreferenced by the program.
     -- However, relying on this behaviour is not generally recommended:
     -- the garbage collector is unpredictable.  If possible, use
     -- an explicit 'hClose' to close 'Handle's when they are no longer
     -- required.  GHC does not currently attempt to free up file
     -- descriptors when they have run out, it is your responsibility to
     -- ensure that this doesn't happen.

     -- **  Standard handles
     -- |  Three handles are allocated during program initialisation,
     -- and are initially open.
     stdin,
     stdout,
     stderr,
     -- *  Opening and closing files
     -- **  Opening files
     withFile,
     openFile,
     IOMode(ReadMode, WriteMode, AppendMode, ReadWriteMode),
     -- **  Closing files
     hClose,
     -- **  Special cases
     -- |  These functions are also exported by the "Prelude".
     readFile,
     readFile',
     writeFile,
     appendFile,
     -- **  File locking
     -- $locking
     -- *  Operations on handles
     -- **  Determining and changing the size of a file
     hFileSize,
     hSetFileSize,
     -- **  Detecting the end of input
     hIsEOF,
     isEOF,
     -- **  Buffering operations
     BufferMode(NoBuffering, LineBuffering, BlockBuffering),
     hSetBuffering,
     hGetBuffering,
     hFlush,
     -- **  Repositioning handles
     hGetPosn,
     hSetPosn,
     HandlePosn,
     hSeek,
     SeekMode(AbsoluteSeek, RelativeSeek, SeekFromEnd),
     hTell,
     -- **  Handle properties
     hIsOpen,
     hIsClosed,
     hIsReadable,
     hIsWritable,
     hIsSeekable,
     -- **  Terminal operations (not portable: GHC only)
     hIsTerminalDevice,
     hSetEcho,
     hGetEcho,
     -- **  Showing handle state (not portable: GHC only)
     hShow,
     -- *  Text input and output
     -- **  Text input
     hWaitForInput,
     hReady,
     hGetChar,
     hGetLine,
     hLookAhead,
     hGetContents,
     hGetContents',
     -- **  Text output
     hPutChar,
     hPutStr,
     hPutStrLn,
     hPrint,
     -- **  Special cases for standard input and output
     -- |  These functions are also exported by the "Prelude".
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
     -- *  Binary input and output
     withBinaryFile,
     openBinaryFile,
     hSetBinaryMode,
     hPutBuf,
     hGetBuf,
     hGetBufSome,
     hPutBufNonBlocking,
     hGetBufNonBlocking,
     -- *  Temporary files
     openTempFile,
     openBinaryTempFile,
     openTempFileWithDefaultPermissions,
     openBinaryTempFileWithDefaultPermissions,
     -- *  Unicode encoding\/decoding
     -- |  A text-mode 'Handle' has an associated 'TextEncoding', which
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
     -- **  Unicode encodings
     TextEncoding,
     latin1,
     utf8,
     utf8_bom,
     utf16,
     utf16le,
     utf16be,
     utf32,
     utf32le,
     utf32be,
     localeEncoding,
     char8,
     mkTextEncoding,
     -- *  Newline conversion
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

     hSetNewlineMode,
     Newline(..),
     nativeNewline,
     NewlineMode(..),
     noNewlineTranslation,
     universalNewlineMode,
     nativeNewlineMode
     ) where

import GHC.Internal.System.IO

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
-- failure with 'GHC.Internal.System.IO.Error.isAlreadyInUseError'.

-- $stdio_examples
-- Note: Some of the examples in this module do not work "as is" in ghci.
-- This is because using 'stdin' in combination with lazy IO
-- does not work well in interactive mode.
--
-- Lines starting with @>@ indicate 'stdin' and @^D@ signales EOF.
--
-- ==== __Example__
--
-- >>> foo
-- > input
-- output
-- > input^D
-- output
