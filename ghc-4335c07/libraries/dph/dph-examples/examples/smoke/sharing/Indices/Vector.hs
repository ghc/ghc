{-# LANGUAGE BangPatterns #-}
module Vector (treeLookup) where
import Data.Vector.Unboxed              (Vector)
import qualified Data.Vector.Unboxed    as U


treeLookup :: Vector Int -> Vector Int -> Vector Int
treeLookup table xx
 | U.length xx == 1
 = U.singleton (table U.! (xx U.! 0))
        
 | otherwise
 = let   len     = U.length xx
         half    = len `div` 2
         s1      = U.slice 0    half xx
         s2      = U.slice half half xx           
   in    U.concat (map (treeLookup table) [s1, s2])
