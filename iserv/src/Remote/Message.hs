{-# LANGUAGE GADTs, StandaloneDeriving, ExistentialQuantification #-}

module Remote.Message
  ( SlaveMessage(..)
  , SlaveMsg(..)
  , sha256sum
  , putSlaveMessage
  , getSlaveMessage )
where

import Data.Binary
import Data.ByteString as BS (ByteString, readFile)

import Crypto.Hash

type Sha256Hash = String

sha256 :: ByteString -> Digest SHA256
sha256 = hash

sha256sum :: FilePath -> IO Sha256Hash
sha256sum path = (show . sha256) <$> BS.readFile path

-- | A @SlaveMessage a@ is message from the iserv process on the
-- target, requesting something from the Proxy of with result type @a@.
data SlaveMessage a where
  -- sends either a new file, or nothing if the file is acceptable.
  Have     :: FilePath -> Sha256Hash -> SlaveMessage (Maybe ByteString)
  Missing  :: FilePath -> SlaveMessage ByteString
  Done     :: SlaveMessage ()

deriving instance Show (SlaveMessage a)

putSlaveMessage :: SlaveMessage a -> Put
putSlaveMessage m = case m of
  Have path sha  -> putWord8 0 >> put path >> put sha
  Missing path   -> putWord8 1 >> put path
  Done           -> putWord8 2

data SlaveMsg = forall a . (Binary a, Show a) => SlaveMsg (SlaveMessage a)

getSlaveMessage :: Get SlaveMsg
getSlaveMessage = do
  b <- getWord8
  case b of
    0 -> SlaveMsg <$> (Have   <$> get <*> get)
    1 -> SlaveMsg <$> Missing <$> get
    2 -> return (SlaveMsg Done)
