
-- | Parallel array data.
--
--   This is an interface onto the internal array types and operators defined
--   by the library, and should not normally be used by client programs.
module Data.Array.Parallel.PArray.PData 
        ( -- * Parallel array types
          PArray (..), PData(..), PDatas(..)
        , length, takeData
        
          -- * PR (Parallel Representation)
        , PR (..)        

          -- * Extra conversions
        , fromListPR
        , toListPR

          -- * Nested arrays
        , module Data.Array.Parallel.PArray.PData.Nested

          -- * Tuple arrays
        , module Data.Array.Parallel.PArray.PData.Tuple2
        , module Data.Array.Parallel.PArray.PData.Tuple3
        , module Data.Array.Parallel.PArray.PData.Tuple4
        , module Data.Array.Parallel.PArray.PData.Tuple5
        , module Data.Array.Parallel.PArray.PData.Tuple6)
where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Wrap
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.PArray.PData.Tuple2
import Data.Array.Parallel.PArray.PData.Tuple3
import Data.Array.Parallel.PArray.PData.Tuple4
import Data.Array.Parallel.PArray.PData.Tuple5
import Data.Array.Parallel.PArray.PData.Tuple6
import Data.Array.Parallel.PArray.PData.Tuple7
import Data.Array.Parallel.PArray.PData.Void            ()
import Data.Array.Parallel.PArray.PData.Unit            ()
import Data.Array.Parallel.PArray.PData.Int             ()
import Data.Array.Parallel.PArray.PData.Word8           ()
import Data.Array.Parallel.PArray.PData.Double          ()
import Data.Array.Parallel.PArray.PData.Sum2            ()
import Data.Array.Parallel.PArray.PRepr.Instances       ()
import qualified Data.Vector                            as V
import Prelude hiding (length)


-- | Convert a list to a PData.
fromListPR :: PR a => [a] -> PData a
fromListPR      = fromVectorPR . V.fromList 


-- | Convert a PData to a list.
toListPR :: PR a => PData a -> [a]
toListPR        = V.toList . toVectorPR

