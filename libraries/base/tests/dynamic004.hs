module Main where

import Data.Typeable
import Data.Typeable.Internal
import GHC.Fingerprint
import Text.Printf

f :: Typeable a => Int -> a -> [TypeRep]
f 0 a = []
f n a = typeOf a : f (n-1) [a]

-- pointwise compare 1000x1001 TypeReps, there should be exactly 1000 equalities
-- (can be used as a benchmark)
main = print $ length [ t1 | t1 <- f 1000 (), t2 <- f 1001 (), t1 == t2 ]

{-
 DEBUGGING code to help find bugs in the TypeRep implementation when
 this test fails:

 where
   g (x:xs) (y:ys)
     | x == y = g xs ys
     | otherwise = do
         print x
         case x of
           TypeRep f1 (TyCon f2 _ _ _) [TypeRep f3 _ _] ->
              printf "f1: %s\nf2: %s\nf3: %s\n" (show_fp f1) (show_fp f2) (show_fp f3)
         case y of
           TypeRep f1 (TyCon f2 _ _ _) [TypeRep f3 _ _] ->
              printf "f1: %s\nf2: %s\nf3: %s\n" (show_fp f1) (show_fp f2) (show_fp f3)
   g _ _ = return ()

   show_fp :: Fingerprint -> String
   show_fp (Fingerprint h l) =
       printf "%x %x" h l
-}
