
{-# OPTIONS -fno-implicit-prelude #-}

module Foreign.Storable where

import GHC.Float
import GHC.Int
import GHC.Num
import GHC.Word

class Storable a

instance Storable Int8
instance Storable Int16
instance Storable Int32
instance Storable Int64
instance Storable Word8
instance Storable Word16
instance Storable Word32
instance Storable Word64
instance Storable Float
instance Storable Double

