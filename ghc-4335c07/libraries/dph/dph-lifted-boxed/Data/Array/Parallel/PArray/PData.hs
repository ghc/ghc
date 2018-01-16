
module Data.Array.Parallel.PArray.PData
        ( PArray (..), PData (..), PDatas (..)
        , PR(..))
where
import Data.Array.Parallel.PArray.Types
import Data.Vector                              (Vector)
import qualified Data.Vector                    as V
import GHC.Exts
import Data.Word

-------------------------------------------------------------------------------
-- | Parallel arrays.
data PArray a
        = PArray Int# (PData a)

-- | A chunk of parallel array data.
data family PData a

-- | An array of chunks of parallel array data.
data family PDatas a


-- | Class of element types that we can store in parallel arrays.
class PR a where
  fromVectorPR :: Vector a -> PData  a
  toVectorPR   :: PData a  -> Vector a


-- Void
data instance PData  Void       = PVoid   (Vector Void)
data instance PDatas Void       = PVoids  (Vector (PData Void))

instance PR Void where
  fromVectorPR vec              = PVoid vec
  toVectorPR   (PVoid vec)      = vec


-- Unit
data instance PData ()          = PUnit   (Vector ())
data instance PDatas ()         = PUnits  (Vector (PData ()))

instance PR () where
  fromVectorPR vec              = PUnit vec
  toVectorPR   (PUnit vec)      = vec


-- Int
data instance PData  Int        = PInt    (Vector Int)
data instance PDatas Int        = PInts   (Vector (PData Int))

instance PR Int where
  fromVectorPR vec              = PInt vec
  toVectorPR   (PInt vec)       = vec


-- Double
data instance PData  Double     = PDouble  (Vector Double)
data instance PDatas Double     = PDoubles (Vector (PData Double))

instance PR Double where
  fromVectorPR vec              = PDouble vec
  toVectorPR   (PDouble vec)    = vec


-- Word8
data instance PData  Word8      = PWord8  (Vector Word8)
data instance PDatas Word8      = PWord8s (Vector (PData Word8))

instance PR Word8 where
  fromVectorPR vec              = PWord8 vec
  toVectorPR   (PWord8 vec)     = vec

-- Bool
data instance PData  Bool       = PBool  (Vector Bool)
data instance PDatas Bool       = PBools (Vector (PData Bool))

instance PR Bool where
  fromVectorPR vec              = PBool vec
  toVectorPR   (PBool vec)      = vec


-- PArray
data instance PData  (PArray a) = PNested  (Vector (PArray a))
data instance PDatas (PArray a) = PNesteds (Vector (PData (PArray a)))

instance PR a => PR (PArray a) where
  fromVectorPR vec              = PNested vec
  toVectorPR   (PNested vec)    = vec


-- Tuple2
data instance PData  (a, b)     = PTuple2  (PData a)  (PData b)
data instance PDatas (a, b)     = PTuple2s (PDatas a) (PDatas b)

instance (PR a, PR b) => PR (a, b) where
  fromVectorPR vec      
   = let (as, bs)       = V.unzip vec
     in  PTuple2 (fromVectorPR as) (fromVectorPR bs)

  toVectorPR   (PTuple2 as bs)  
   = V.zip (toVectorPR as) (toVectorPR bs)


