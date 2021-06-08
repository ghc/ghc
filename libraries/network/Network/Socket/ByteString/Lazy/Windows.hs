{-# LANGUAGE OverloadedStrings #-}

module Network.Socket.ByteString.Lazy.Windows (
    -- * Send data to a socket
    send
  , sendAll
  ) where

import qualified Data.ByteString           as S
import qualified Data.ByteString.Lazy      as L
import qualified Network.Socket.ByteString as Socket
import           Network.Socket.Imports
import           Network.Socket.ByteString.IO       (waitWhen0)
import           Network.Socket.Types

-- -----------------------------------------------------------------------------
-- Sending
send
    :: Socket -- ^ Connected socket
    -> L.ByteString -- ^ Data to send
    -> IO Int64 -- ^ Number of bytes sent
send s lbs = case L.toChunks lbs of
    -- TODO: Consider doing nothing if the string is empty.
    []    -> fromIntegral <$> Socket.send s S.empty
    (x:_) -> fromIntegral <$> Socket.send s x

sendAll
    :: Socket -- ^ Connected socket
    -> L.ByteString -- ^ Data to send
    -> IO ()
sendAll _ "" = return ()
sendAll s bs = do
    sent <- send s bs
    waitWhen0 (fromIntegral sent) s
    when (sent >= 0) $ sendAll s $ L.drop sent bs
