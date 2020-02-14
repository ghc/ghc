{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Socket.ByteString.Lazy.Posix (
    -- * Send data to a socket
    send
  , sendAll
  ) where

import qualified Data.ByteString.Lazy               as L
import           Data.ByteString.Unsafe             (unsafeUseAsCStringLen)
import           Foreign.Marshal.Array              (allocaArray)

import           Network.Socket.ByteString.Internal (c_writev)
import           Network.Socket.ByteString.IO       (waitWhen0)
import           Network.Socket.ByteString.IOVec    (IOVec (IOVec))
import           Network.Socket.Imports
import           Network.Socket.Internal
import           Network.Socket.Types

-- -----------------------------------------------------------------------------
-- Sending
send
    :: Socket -- ^ Connected socket
    -> L.ByteString -- ^ Data to send
    -> IO Int64 -- ^ Number of bytes sent
send s lbs = do
    let cs  = take maxNumChunks (L.toChunks lbs)
        len = length cs
    siz <- withFdSocket s $ \fd -> allocaArray len $ \ptr ->
             withPokes cs ptr $ \niovs ->
               throwSocketErrorWaitWrite s "writev" $ c_writev fd ptr niovs
    return $ fromIntegral siz
  where
    withPokes ss p f = loop ss p 0 0
      where
        loop (c:cs) q k !niovs
            | k < maxNumBytes = unsafeUseAsCStringLen c $ \(ptr, len) -> do
                poke q $ IOVec ptr (fromIntegral len)
                loop cs
                     (q `plusPtr` sizeOf (undefined :: IOVec))
                     (k + fromIntegral len)
                     (niovs + 1)
            | otherwise = f niovs
        loop _ _ _ niovs = f niovs
    maxNumBytes  = 4194304 :: Int -- maximum number of bytes to transmit in one system call
    maxNumChunks = 1024 :: Int -- maximum number of chunks to transmit in one system call

sendAll
    :: Socket -- ^ Connected socket
    -> L.ByteString -- ^ Data to send
    -> IO ()
sendAll _ "" = return ()
sendAll s bs = do
    sent <- send s bs
    waitWhen0 (fromIntegral sent) s
    when (sent >= 0) $ sendAll s $ L.drop sent bs
