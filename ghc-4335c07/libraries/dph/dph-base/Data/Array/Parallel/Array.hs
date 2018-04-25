
-- | Generic array class.
--   This is used as a compatability layer during testing and debugging.
module Data.Array.Parallel.Array 
        ( Array(..)
        , fromList, toList
        , toVectors1,   toVectors2,   toVectors3
        , fromVectors1, fromVectors2, fromVectors3)
where   
import Control.Monad
import Data.Vector              (Vector)
import qualified Data.Vector    as V
import qualified Prelude        as P
import Prelude                  hiding (length)


class Array a e where
 -- | Check whether an array has a valid internal representation.
 valid      :: a e -> Bool

 -- | Yield an array with just a single element.
 singleton  :: e   -> a e

 -- | Append two arrays.
 append     :: a e -> a e -> a e

 -- | Yield the length of an array.
 length     :: a e -> Int

 -- | Retrieve the element at the given index. 
 index      :: a e -> Int -> e

 -- | Convert an array to a vector.
 toVector   :: a e -> Vector e

 -- | Convert a vector to an array.
 fromVector :: Vector e -> a e
 

instance Array [] e where
 valid          = const True
 singleton x    = [x]
 length         = P.length
 index          = (P.!!)
 append         = (P.++)
 toVector       = V.fromList
 fromVector     = V.toList
 

instance Array Vector e where
 valid          = const True
 singleton      = V.singleton
 length         = V.length
 index          = (V.!)
 append         = (V.++)
 toVector       = id
 fromVector     = id


-- | Convert a list to an array.
fromList :: Array a e => [e] -> a e
fromList = fromVector . V.fromList


-- | Convert an array to a list.
toList   :: Array a e => a e -> [e]
toList   = V.toList . toVector


-- | Convert the outer level of an array to vectors.
toVectors1 
        :: Array a e
        => a e -> Vector e

toVectors1 arr
        = toVector arr
        
        
-- | Convert the outer two levels of an array to vectors.
toVectors2 
        :: (Array a1 (a2 e), Array a2 e)
        => a1 (a2 e) -> Vector (Vector e)

toVectors2 = V.map toVector . toVector
        

-- | Convert the outer three levels of an array to vectors.
toVectors3
        :: (Array a1 (a2 (a3 e)), Array a2 (a3 e), Array a3 e)
        => a1 (a2 (a3 e)) -> Vector (Vector (Vector e))

toVectors3 = V.map (V.map toVector) . V.map toVector . toVector 
        

-- | Convert some vectors to an array.
fromVectors1 
        :: Array a e
        => Vector e -> a e

fromVectors1 vec
        = fromVector vec
        

-- | Convert some vectors to a nested array
fromVectors2 
        :: (Array a1 (a2 e), Array a2 e)
        => Vector (Vector e) -> a1 (a2 e)

fromVectors2
        = fromVector . V.map fromVector


-- | Convert some vectors to a triply nested array
fromVectors3 
        :: (Array a1 (a2 (a3 e)), Array a2 (a3 e), Array a3 e)
        => Vector (Vector (Vector e)) -> a1 (a2 (a3 e))

fromVectors3
        = fromVector . V.map fromVector . V.map (V.map fromVector)

   
