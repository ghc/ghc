
module Vector (treeReverse) where
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed              (Vector)

-- | Reverse the elements in an array using a tree.
treeReverse :: Vector Int -> Vector Int
{-# NOINLINE treeReverse #-}
treeReverse xx
        | V.length xx == 1
        = xx
        
        | otherwise
        = let   len     = V.length xx
                half    = len `div` 2
                s1      = V.slice 0    half  xx
                s2      = V.slice half half  xx         
          in    treeReverse s2 V.++ treeReverse s1

