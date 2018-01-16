{-# LANGUAGE BangPatterns, CPP, RecordWildCards #-}
-- |
-- Module      : Data.Text.Internal.IO
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Simon Marlow
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Low-level support for text I\/O.

module Data.Text.Internal.IO
    (
      hGetLineWith
    , readChunk
    ) where

import qualified Control.Exception as E
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.Internal.Fusion (unstream)
import Data.Text.Internal.Fusion.Types (Step(..), Stream(..))
import Data.Text.Internal.Fusion.Size (exactSize, maxSize)
import Data.Text.Unsafe (inlinePerformIO)
import Foreign.Storable (peekElemOff)
import GHC.IO.Buffer (Buffer(..), CharBuffer, RawCharBuffer, bufferAdjustL,
                      bufferElems, charSize, isEmptyBuffer, readCharBuf,
                      withRawBuffer, writeCharBuf)
import GHC.IO.Handle.Internals (ioe_EOF, readTextDevice, wantReadableHandle_)
import GHC.IO.Handle.Types (Handle__(..), Newline(..))
import System.IO (Handle)
import System.IO.Error (isEOFError)
import qualified Data.Text as T

-- | Read a single line of input from a handle, constructing a list of
-- decoded chunks as we go.  When we're done, transform them into the
-- destination type.
hGetLineWith :: ([Text] -> t) -> Handle -> IO t
hGetLineWith f h = wantReadableHandle_ "hGetLine" h go
  where
    go hh@Handle__{..} = readIORef haCharBuffer >>= fmap f . hGetLineLoop hh []

hGetLineLoop :: Handle__ -> [Text] -> CharBuffer -> IO [Text]
hGetLineLoop hh@Handle__{..} = go where
 go ts buf@Buffer{ bufL=r0, bufR=w, bufRaw=raw0 } = do
  let findEOL raw r | r == w    = return (False, w)
                    | otherwise = do
        (c,r') <- readCharBuf raw r
        if c == '\n'
          then return (True, r)
          else findEOL raw r'
  (eol, off) <- findEOL raw0 r0
  (t,r') <- if haInputNL == CRLF
            then unpack_nl raw0 r0 off
            else do t <- unpack raw0 r0 off
                    return (t,off)
  if eol
    then do writeIORef haCharBuffer (bufferAdjustL (off+1) buf)
            return $ reverse (t:ts)
    else do
      let buf1 = bufferAdjustL r' buf
      maybe_buf <- maybeFillReadBuffer hh buf1
      case maybe_buf of
         -- Nothing indicates we caught an EOF, and we may have a
         -- partial line to return.
         Nothing -> do
              -- we reached EOF.  There might be a lone \r left
              -- in the buffer, so check for that and
              -- append it to the line if necessary.
              let pre | isEmptyBuffer buf1 = T.empty
                      | otherwise          = T.singleton '\r'
              writeIORef haCharBuffer buf1{ bufL=0, bufR=0 }
              let str = reverse . filter (not . T.null) $ pre:t:ts
              if null str
                then ioe_EOF
                else return str
         Just new_buf -> go (t:ts) new_buf

-- This function is lifted almost verbatim from GHC.IO.Handle.Text.
maybeFillReadBuffer :: Handle__ -> CharBuffer -> IO (Maybe CharBuffer)
maybeFillReadBuffer handle_ buf
  = E.catch (Just `fmap` getSomeCharacters handle_ buf) $ \e ->
      if isEOFError e
      then return Nothing
      else ioError e

unpack :: RawCharBuffer -> Int -> Int -> IO Text
unpack !buf !r !w
 | charSize /= 4 = sizeError "unpack"
 | r >= w        = return T.empty
 | otherwise     = withRawBuffer buf go
 where
  go pbuf = return $! unstream (Stream next r (exactSize (w-r)))
   where
    next !i | i >= w    = Done
            | otherwise = Yield (ix i) (i+1)
    ix i = inlinePerformIO $ peekElemOff pbuf i

unpack_nl :: RawCharBuffer -> Int -> Int -> IO (Text, Int)
unpack_nl !buf !r !w
 | charSize /= 4 = sizeError "unpack_nl"
 | r >= w        = return (T.empty, 0)
 | otherwise     = withRawBuffer buf $ go
 where
  go pbuf = do
    let !t = unstream (Stream next r (maxSize (w-r)))
        w' = w - 1
    return $ if ix w' == '\r'
             then (t,w')
             else (t,w)
   where
    next !i | i >= w = Done
            | c == '\r' = let i' = i + 1
                          in if i' < w
                             then if ix i' == '\n'
                                  then Yield '\n' (i+2)
                                  else Yield '\n' i'
                             else Done
            | otherwise = Yield c (i+1)
            where c = ix i
    ix i = inlinePerformIO $ peekElemOff pbuf i

-- This function is completely lifted from GHC.IO.Handle.Text.
getSomeCharacters :: Handle__ -> CharBuffer -> IO CharBuffer
getSomeCharacters handle_@Handle__{..} buf@Buffer{..} =
  case bufferElems buf of
    -- buffer empty: read some more
    0 -> {-# SCC "readTextDevice" #-} readTextDevice handle_ buf

    -- if the buffer has a single '\r' in it and we're doing newline
    -- translation: read some more
    1 | haInputNL == CRLF -> do
      (c,_) <- readCharBuf bufRaw bufL
      if c == '\r'
         then do -- shuffle the '\r' to the beginning.  This is only safe
                 -- if we're about to call readTextDevice, otherwise it
                 -- would mess up flushCharBuffer.
                 -- See [note Buffer Flushing], GHC.IO.Handle.Types
                 _ <- writeCharBuf bufRaw 0 '\r'
                 let buf' = buf{ bufL=0, bufR=1 }
                 readTextDevice handle_ buf'
         else do
                 return buf

    -- buffer has some chars in it already: just return it
    _otherwise -> {-# SCC "otherwise" #-} return buf

-- | Read a single chunk of strict text from a buffer. Used by both
-- the strict and lazy implementations of hGetContents.
readChunk :: Handle__ -> CharBuffer -> IO Text
readChunk hh@Handle__{..} buf = do
  buf'@Buffer{..} <- getSomeCharacters hh buf
  (t,r) <- if haInputNL == CRLF
           then unpack_nl bufRaw bufL bufR
           else do t <- unpack bufRaw bufL bufR
                   return (t,bufR)
  writeIORef haCharBuffer (bufferAdjustL r buf')
  return t

sizeError :: String -> a
sizeError loc = error $ "Data.Text.IO." ++ loc ++ ": bad internal buffer size"
