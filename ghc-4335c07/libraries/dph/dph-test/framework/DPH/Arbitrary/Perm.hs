
-- | Generation of arbitrary permutations.
module DPH.Arbitrary.Perm
        (Perm (..))
where
import Test.QuickCheck
import qualified Data.Vector    as V
import Data.Vector              (Vector)
import Data.List


-- | Permutation of [0..n-1] with all values appearing exactly once.
data Perm 
        = Perm (Vector Int)
        deriving (Eq, Show)


instance Arbitrary Perm where
  arbitrary 
   = sized $ \n -> (Perm . V.fromList) `fmap` (permute [0..n-1])
   where 
      -- generate a random permutation of the given list
      permute :: [Int] -> Gen [Int]
      permute [] = return []
      permute xs 
       = do     -- choose random element of the list and place it at head
                x    <- elements xs
                rest <- permute (delete x xs)
                return (x : rest)

