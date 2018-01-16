{-# LANGUAGE MagicHash, DataKinds, PolyKinds, TypeInType, TypeFamilies #-}

module T13244 where

import Data.Int
import Data.Word
import GHC.Prim
import GHC.Types

type family Rep x where
  Rep Int    = IntRep
  Rep Int8   = IntRep
  Rep Int16  = IntRep
  Rep Int32  = IntRep
  Rep Int64  = Int64Rep
  Rep Bool   = IntRep
  Rep Char   = IntRep
  Rep Word   = WordRep
  Rep Word8  = WordRep
  Rep Word16 = WordRep
  Rep Word32 = WordRep
  Rep Word64 = Word64Rep
  Rep Float  = FloatRep
  Rep Double = DoubleRep

class Unbox x where
  type Unboxed x :: TYPE (Rep x)
  unbox :: x -> Unboxed x
  box :: Unboxed x -> x

instance Unbox Int where
  type Unboxed Int = Int#
  unbox (I# i) = i
  box = I#
