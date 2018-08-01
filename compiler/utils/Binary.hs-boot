module Binary where

import Binary.Class (Binary)
import GhcPrelude
import Data.Word
import Data.Int
import Data.ByteString (ByteString)
import FastString

instance Binary Word8
instance Binary Word16
instance Binary Word32
instance Binary Word64
instance Binary Int8
instance Binary Int16
instance Binary Int32
instance Binary Int64
instance Binary ()
instance Binary Bool
instance Binary Char
instance Binary Int
instance Binary a => Binary [a]
instance (Binary a, Binary b) => Binary (a, b)

instance Binary ByteString
instance Binary FastString