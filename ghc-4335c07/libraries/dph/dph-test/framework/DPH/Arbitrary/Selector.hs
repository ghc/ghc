
-- | Generation of arbitrary selectors for the combine opeartor.
module DPH.Arbitrary.Selector
        (Selector (..))
where
import DPH.Arbitrary.Perm
import Test.QuickCheck
import qualified Data.Vector    as V
import Data.Vector              (Vector)


-- | A selector for the combine operation.
--   The selector is an even-lengthed vector of 0's and 1's,
--   with the same number of 0's and 1's.
data Selector
        = Selector (Vector Int)
        deriving (Eq, Show)


instance Arbitrary Selector where
 arbitrary
  = do  
        -- Make a permutation that says where each element of the selector should go.
        -- The selector length needs to be even, and so does the permutation.  
        Perm perm'  <- arbitrary
        let perm    = if V.length perm' `mod` 2 == 1
                        then perm' V.++ (V.singleton $ V.length perm')
                        else perm'
         
        let vecLen      = V.length perm `div` 2

        -- We've got the same number of 0's and 1's,
        -- but mixed up according to the permutation.
        let vecTags     = V.backpermute
                                (V.replicate vecLen 0  V.++  V.replicate vecLen (1 :: Int))
                                perm

        return (Selector vecTags)
