
-- | Generation of arbitrary segment descriptors.
module DPH.Arbitrary.Segd
        ( segdForArray
        , checkSegd)
where
import Test.QuickCheck
import Data.Array.Parallel.Unlifted as U hiding ( update )
import Data.List
import Prelude as P


-- | Segment descriptor of length n.
-- 
--   Do not use directly unless an arbitrary Segd is all you need.  
--   Use segdForArray to generate a Segd that fits the array.
-- 
instance Arbitrary Segd where
  arbitrary = sized $ \n -> 
    do
      ids <- genIndices n
      let lens = indicesToLengths ids n
      return $ mkSegd (fromList lens) (fromList ids) n
    where 
      -- list of non-decreasing integers in range [0, n)
      genIndices 0 = return []
      genIndices n = ((0:) . sort . P.map (`mod` n)) `fmap` arbitrary
      indicesToLengths ids n = P.zipWith (-) (tail $ ids ++ [n]) ids


-- | Generate a segment descriptor fitting the given array
segdForArray :: (Elt a) => Array a -> Gen Segd
segdForArray arr = resize (U.length arr) arbitrary


-- | Consistency check for a segment descriptor against a provided list of lengths
checkSegd :: Segd -> Array Int -> Bool
checkSegd segd lens =
     (lengthsSegd  segd == lens)
  && (indicesSegd  segd == U.scan (+) 0 lens)
  && (elementsSegd segd == U.sum lens)
