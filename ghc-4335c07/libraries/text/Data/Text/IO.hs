{-# LANGUAGE BangPatterns, CPP, RecordWildCards, ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- |
-- Module      : Data.Text.IO
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Simon Marlow
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Efficient locale-sensitive support for text I\/O.
--
-- Skip past the synopsis for some important notes on performance and
-- portability across different versions of GHC.

module Data.Text.IO
    (
    -- * Performance
    -- $performance

    -- * Locale support
    -- $locale
    -- * File-at-a-time operations
      readFile
    , writeFile
    , appendFile
    -- * Operations on handles
    , hGetContents
    , hGetChunk
    , hGetLine
    , hPutStr
    , hPutStrLn
    -- * Special cases for standard input and output
    , interact
    , getContents
    , getLine
    , putStr
    , putStrLn
    ) where

import Data.Text (Text)
import Prelude hiding (appendFile, getContents, getLine, interact,
                       putStr, putStrLn, readFile, writeFile)
import System.IO (Handle, IOMode(..), hPutChar, openFile, stdin, stdout,
                  withFile)
import qualified Control.Exception as E
import Control.Monad (liftM2, when)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Text as T
import Data.Text.Internal.Fusion (stream)
import Data.Text.Internal.Fusion.Types (Step(..), Stream(..))
import Data.Text.Internal.IO (hGetLineWith, readChunk)
import GHC.IO.Buffer (Buffer(..), BufferState(..), CharBufElem, CharBuffer,
                      RawCharBuffer, emptyBuffer, isEmptyBuffer, newCharBuffer,
                      writeCharBuf)
import GHC.IO.Exception (IOException(ioe_type), IOErrorType(InappropriateType))
import GHC.IO.Handle.Internals (augmentIOError, hClose_help, wantReadableHandle,
                                wantWritableHandle)
import GHC.IO.Handle.Text (commitBuffer')
import GHC.IO.Handle.Types (BufferList(..), BufferMode(..), Handle__(..),
                            HandleType(..), Newline(..))
import System.IO (hGetBuffering, hFileSize, hSetBuffering, hTell)
import System.IO.Error (isEOFError)

-- $performance
-- #performance#
--
-- The functions in this module obey the runtime system's locale,
-- character set encoding, and line ending conversion settings.
--
-- If you know in advance that you will be working with data that has
-- a specific encoding (e.g. UTF-8), and your application is highly
-- performance sensitive, you may find that it is faster to perform
-- I\/O with bytestrings and to encode and decode yourself than to use
-- the functions in this module.
--
-- Whether this will hold depends on the version of GHC you are using,
-- the platform you are working on, the data you are working with, and
-- the encodings you are using, so be sure to test for yourself.

-- | The 'readFile' function reads a file and returns the contents of
-- the file as a string.  The entire file is read strictly, as with
-- 'getContents'.
readFile :: FilePath -> IO Text
readFile name = openFile name ReadMode >>= hGetContents

-- | Write a string to a file.  The file is truncated to zero length
-- before writing begins.
writeFile :: FilePath -> Text -> IO ()
writeFile p = withFile p WriteMode . flip hPutStr

-- | Write a string the end of a file.
appendFile :: FilePath -> Text -> IO ()
appendFile p = withFile p AppendMode . flip hPutStr

catchError :: String -> Handle -> Handle__ -> IOError -> IO Text
catchError caller h Handle__{..} err
    | isEOFError err = do
        buf <- readIORef haCharBuffer
        return $ if isEmptyBuffer buf
                 then T.empty
                 else T.singleton '\r'
    | otherwise = E.throwIO (augmentIOError err caller h)

-- | /Experimental./ Read a single chunk of strict text from a
-- 'Handle'. The size of the chunk depends on the amount of input
-- currently buffered.
--
-- This function blocks only if there is no data available, and EOF
-- has not yet been reached. Once EOF is reached, this function
-- returns an empty string instead of throwing an exception.
hGetChunk :: Handle -> IO Text
hGetChunk h = wantReadableHandle "hGetChunk" h readSingleChunk
 where
  readSingleChunk hh@Handle__{..} = do
    buf <- readIORef haCharBuffer
    t <- readChunk hh buf `E.catch` catchError "hGetChunk" h hh
    return (hh, t)

-- | Read the remaining contents of a 'Handle' as a string.  The
-- 'Handle' is closed once the contents have been read, or if an
-- exception is thrown.
--
-- Internally, this function reads a chunk at a time from the
-- lower-level buffering abstraction, and concatenates the chunks into
-- a single string once the entire file has been read.
--
-- As a result, it requires approximately twice as much memory as its
-- result to construct its result.  For files more than a half of
-- available RAM in size, this may result in memory exhaustion.
hGetContents :: Handle -> IO Text
hGetContents h = do
  chooseGoodBuffering h
  wantReadableHandle "hGetContents" h readAll
 where
  readAll hh@Handle__{..} = do
    let readChunks = do
          buf <- readIORef haCharBuffer
          t <- readChunk hh buf `E.catch` catchError "hGetContents" h hh
          if T.null t
            then return [t]
            else (t:) `fmap` readChunks
    ts <- readChunks
    (hh', _) <- hClose_help hh
    return (hh'{haType=ClosedHandle}, T.concat ts)

-- | Use a more efficient buffer size if we're reading in
-- block-buffered mode with the default buffer size.  When we can
-- determine the size of the handle we're reading, set the buffer size
-- to that, so that we can read the entire file in one chunk.
-- Otherwise, use a buffer size of at least 16KB.
chooseGoodBuffering :: Handle -> IO ()
chooseGoodBuffering h = do
  bufMode <- hGetBuffering h
  case bufMode of
    BlockBuffering Nothing -> do
      d <- E.catch (liftM2 (-) (hFileSize h) (hTell h)) $ \(e::IOException) ->
           if ioe_type e == InappropriateType
           then return 16384 -- faster than the 2KB default
           else E.throwIO e
      when (d > 0) . hSetBuffering h . BlockBuffering . Just . fromIntegral $ d
    _ -> return ()

-- | Read a single line from a handle.
hGetLine :: Handle -> IO Text
hGetLine = hGetLineWith T.concat

-- | Write a string to a handle.
hPutStr :: Handle -> Text -> IO ()
-- This function is lifted almost verbatim from GHC.IO.Handle.Text.
hPutStr h t = do
  (buffer_mode, nl) <-
       wantWritableHandle "hPutStr" h $ \h_ -> do
                     bmode <- getSpareBuffer h_
                     return (bmode, haOutputNL h_)
  let str = stream t
  case buffer_mode of
     (NoBuffering, _)        -> hPutChars h str
     (LineBuffering, buf)    -> writeLines h nl buf str
     (BlockBuffering _, buf)
         | nl == CRLF        -> writeBlocksCRLF h buf str
         | otherwise         -> writeBlocksRaw h buf str

hPutChars :: Handle -> Stream Char -> IO ()
hPutChars h (Stream next0 s0 _len) = loop s0
  where
    loop !s = case next0 s of
                Done       -> return ()
                Skip s'    -> loop s'
                Yield x s' -> hPutChar h x >> loop s'

-- The following functions are largely lifted from GHC.IO.Handle.Text,
-- but adapted to a coinductive stream of data instead of an inductive
-- list.
--
-- We have several variations of more or less the same code for
-- performance reasons.  Splitting the original buffered write
-- function into line- and block-oriented versions gave us a 2.1x
-- performance improvement.  Lifting out the raw/cooked newline
-- handling gave a few more percent on top.

writeLines :: Handle -> Newline -> Buffer CharBufElem -> Stream Char -> IO ()
writeLines h nl buf0 (Stream next0 s0 _len) = outer s0 buf0
 where
  outer s1 Buffer{bufRaw=raw, bufSize=len} = inner s1 (0::Int)
   where
    inner !s !n =
      case next0 s of
        Done -> commit n False{-no flush-} True{-release-} >> return ()
        Skip s' -> inner s' n
        Yield x s'
          | n + 1 >= len -> commit n True{-needs flush-} False >>= outer s
          | x == '\n'    -> do
                   n' <- if nl == CRLF
                         then do n1 <- writeCharBuf raw n '\r'
                                 writeCharBuf raw n1 '\n'
                         else writeCharBuf raw n x
                   commit n' True{-needs flush-} False >>= outer s'
          | otherwise    -> writeCharBuf raw n x >>= inner s'
    commit = commitBuffer h raw len

writeBlocksCRLF :: Handle -> Buffer CharBufElem -> Stream Char -> IO ()
writeBlocksCRLF h buf0 (Stream next0 s0 _len) = outer s0 buf0
 where
  outer s1 Buffer{bufRaw=raw, bufSize=len} = inner s1 (0::Int)
   where
    inner !s !n =
      case next0 s of
        Done -> commit n False{-no flush-} True{-release-} >> return ()
        Skip s' -> inner s' n
        Yield x s'
          | n + 1 >= len -> commit n True{-needs flush-} False >>= outer s
          | x == '\n'    -> do n1 <- writeCharBuf raw n '\r'
                               writeCharBuf raw n1 '\n' >>= inner s'
          | otherwise    -> writeCharBuf raw n x >>= inner s'
    commit = commitBuffer h raw len

writeBlocksRaw :: Handle -> Buffer CharBufElem -> Stream Char -> IO ()
writeBlocksRaw h buf0 (Stream next0 s0 _len) = outer s0 buf0
 where
  outer s1 Buffer{bufRaw=raw, bufSize=len} = inner s1 (0::Int)
   where
    inner !s !n =
      case next0 s of
        Done -> commit n False{-no flush-} True{-release-} >> return ()
        Skip s' -> inner s' n
        Yield x s'
          | n + 1 >= len -> commit n True{-needs flush-} False >>= outer s
          | otherwise    -> writeCharBuf raw n x >>= inner s'
    commit = commitBuffer h raw len

-- This function is completely lifted from GHC.IO.Handle.Text.
getSpareBuffer :: Handle__ -> IO (BufferMode, CharBuffer)
getSpareBuffer Handle__{haCharBuffer=ref,
                        haBuffers=spare_ref,
                        haBufferMode=mode}
 = do
   case mode of
     NoBuffering -> return (mode, error "no buffer!")
     _ -> do
          bufs <- readIORef spare_ref
          buf  <- readIORef ref
          case bufs of
            BufferListCons b rest -> do
                writeIORef spare_ref rest
                return ( mode, emptyBuffer b (bufSize buf) WriteBuffer)
            BufferListNil -> do
                new_buf <- newCharBuffer (bufSize buf) WriteBuffer
                return (mode, new_buf)


-- This function is completely lifted from GHC.IO.Handle.Text.
commitBuffer :: Handle -> RawCharBuffer -> Int -> Int -> Bool -> Bool
             -> IO CharBuffer
commitBuffer hdl !raw !sz !count flush release =
  wantWritableHandle "commitAndReleaseBuffer" hdl $
     commitBuffer' raw sz count flush release
{-# INLINE commitBuffer #-}

-- | Write a string to a handle, followed by a newline.
hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn h t = hPutStr h t >> hPutChar h '\n'

-- | The 'interact' function takes a function of type @Text -> Text@
-- as its argument. The entire input from the standard input device is
-- passed to this function as its argument, and the resulting string
-- is output on the standard output device.
interact :: (Text -> Text) -> IO ()
interact f = putStr . f =<< getContents

-- | Read all user input on 'stdin' as a single string.
getContents :: IO Text
getContents = hGetContents stdin

-- | Read a single line of user input from 'stdin'.
getLine :: IO Text
getLine = hGetLine stdin

-- | Write a string to 'stdout'.
putStr :: Text -> IO ()
putStr = hPutStr stdout

-- | Write a string to 'stdout', followed by a newline.
putStrLn :: Text -> IO ()
putStrLn = hPutStrLn stdout

-- $locale
--
-- /Note/: The behaviour of functions in this module depends on the
-- version of GHC you are using.
--
-- Beginning with GHC 6.12, text I\/O is performed using the system or
-- handle's current locale and line ending conventions.
--
-- Under GHC 6.10 and earlier, the system I\/O libraries do not
-- support locale-sensitive I\/O or line ending conversion.  On these
-- versions of GHC, functions in this library all use UTF-8.  What
-- does this mean in practice?
--
-- * All data that is read will be decoded as UTF-8.
--
-- * Before data is written, it is first encoded as UTF-8.
--
-- * On both reading and writing, the platform's native newline
--   conversion is performed.
--
-- If you must use a non-UTF-8 locale on an older version of GHC, you
-- will have to perform the transcoding yourself, e.g. as follows:
--
-- > import qualified Data.ByteString as B
-- > import Data.Text (Text)
-- > import Data.Text.Encoding (encodeUtf16)
-- >
-- > putStr_Utf16LE :: Text -> IO ()
-- > putStr_Utf16LE t = B.putStr (encodeUtf16LE t)
