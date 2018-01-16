{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Selectors. 
--
--   See "Data.Array.Parallel.Unlifted" for how this works.
--
module Data.Array.Parallel.Unlifted.Sequential.USel 
        ( -- * Types
          USel2(..)

          -- * Operations on selectors
        , mkUSel2
        , lengthUSel2
        , tagsUSel2, indicesUSel2
        , elementsUSel2_0, elementsUSel2_1
        , tagsToIndices2)
where
import Data.Array.Parallel.Unlifted.Sequential.Vector   as V
import qualified Data.Vector.Fusion.Bundle              as S
import qualified Data.Vector.Fusion.Bundle.Monadic      as M
import Data.Vector.Fusion.Stream.Monadic                ( Stream(..) )
import Data.Vector.Fusion.Bundle.Monadic                ( Bundle(..) )
import Data.Array.Parallel.Base                         (Tag)


-- | Selector.
data USel2 
        = USel2
        { usel2_tags      :: !(Vector Tag)
        , usel2_indices   :: !(Vector Int)
        , usel2_elements0 :: !Int       -- ^ Number of tags with value 0.
        , usel2_elements1 :: !Int       -- ^ Number of tags with value 1.
        }


-- | O(1). Construct a selector.
mkUSel2 :: Vector Tag           -- ^ Tags array.
        -> Vector Int           -- ^ Indices array
        -> Int                  -- ^ Number of elements taken from first array.
        -> Int                  -- ^ Number of elements taken from second array.
        -> USel2
mkUSel2 = USel2
{-# INLINE mkUSel2 #-}


-- Projections ----------------------------------------------------------------
-- INLINE trivial projections as they'll expand to a single record selector.

-- | O(1). Get the number of elements represented by this selector.
--         This is the length of the array returned by `combine`.
lengthUSel2 :: USel2 -> Int
lengthUSel2     = V.length . usel2_tags
{-# INLINE lengthUSel2 #-}


-- | O(1). Get the tags array of a selector.
tagsUSel2 :: USel2 -> Vector Tag
{-# INLINE tagsUSel2 #-}
tagsUSel2       = usel2_tags


-- | O(1). Get the indices array of a selector.
indicesUSel2 :: USel2 -> Vector Int
indicesUSel2    = usel2_indices
{-# INLINE indicesUSel2 #-}


-- | O(1). Get the number of elements that will be taken from the first array.
elementsUSel2_0 :: USel2 -> Int
elementsUSel2_0 = usel2_elements0
{-# INLINE elementsUSel2_0 #-}


-- | O(1). Get the number of elements that will be taken from the second array.
elementsUSel2_1 :: USel2 -> Int
elementsUSel2_1 = usel2_elements1
{-# INLINE elementsUSel2_1 #-}


-- | O(n). Compute the source index for each element of the result array.
tagsToIndices2 :: Vector Tag -> Vector Int
tagsToIndices2 tags 
  = unstream (mapAccumS add (0,0) (stream tags))
  where
    add (i,j) 0 = ((i+1,j),i)
    add (i,j) _ = ((i,j+1),j)
{-# INLINE_STREAM tagsToIndices2 #-}


mapAccumS :: (acc -> a -> (acc,b)) -> acc -> S.Bundle v a -> S.Bundle v b
mapAccumS f acc0 (Bundle{sElems=Stream step s0,sSize=n})
  = M.fromStream (Stream step' (acc0,s0)) n
  where
   {-# INLINE_INNER step' #-}
   step' (acc,s) 
    = do r <- step s
         case r of
          S.Yield x s' -> let (acc',y) = f acc x
                          in return $ S.Yield y (acc',s')
          S.Skip    s' -> return $ S.Skip (acc,s')
          S.Done       -> return S.Done
{-# INLINE_STREAM mapAccumS #-}
