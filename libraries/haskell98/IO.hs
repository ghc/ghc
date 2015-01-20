{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module IO (
        Handle, HandlePosn,
        IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),
        BufferMode(NoBuffering,LineBuffering,BlockBuffering),
        SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
        stdin, stdout, stderr,
        openFile, hClose, hFileSize, hIsEOF, isEOF,
        hSetBuffering, hGetBuffering, hFlush,
        hGetPosn, hSetPosn, hSeek,
        hWaitForInput, hReady, hGetChar, hGetLine, hLookAhead, hGetContents,
        hPutChar, hPutStr, hPutStrLn, hPrint,
        hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable,
        isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError,
        isFullError, isEOFError,
        isIllegalOperation, isPermissionError, isUserError,
        ioeGetErrorString, ioeGetHandle, ioeGetFileName,
        try, bracket, bracket_,

        -- ...and what the Prelude exports
        IO, FilePath, IOError, ioError, userError, catch, interact,
        putChar, putStr, putStrLn, print, getChar, getLine, getContents,
        readFile, writeFile, appendFile, readIO, readLn
    ) where

import System.IO
import System.IO.Error

-- | The 'bracket' function captures a common allocate, compute, deallocate
-- idiom in which the deallocation step must occur even in the case of an
-- error during computation. This is similar to try-catch-finally in Java.
--
-- This version handles only IO errors, as defined by Haskell 98.
-- The version of @bracket@ in "Control.Exception" handles all exceptions,
-- and should be used instead.

bracket        :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after m = do
        x  <- before
        rs <- try (m x)
        _ <- after x
        case rs of
           Right r -> return r
           Left  e -> ioError e

-- | A variant of 'bracket' where the middle computation doesn't want @x@.
--
-- This version handles only IO errors, as defined by Haskell 98.
-- The version of @bracket_@ in "Control.Exception" handles all exceptions,
-- and should be used instead.

bracket_        :: IO a -> (a -> IO b) -> IO c -> IO c
bracket_ before after m = do
         x  <- before
         rs <- try m
         _ <- after x
         case rs of
            Right r -> return r
            Left  e -> ioError e

-- | The construct 'try' @comp@ exposes IO errors which occur within a
-- computation, and which are not fully handled.
--
-- Non-I\/O exceptions are not caught by this variant; to catch all
-- exceptions, use 'Control.Exception.try' from "Control.Exception".
try     :: IO a -> IO (Either IOError a)
try f   =  catch (do r <- f
                     return (Right r))
                 (return . Left)

