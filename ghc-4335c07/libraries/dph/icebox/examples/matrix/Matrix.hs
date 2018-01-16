{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module Matrix where

import Data.Array.Parallel.Prelude
import qualified Data.Array.Parallel.Prelude.Int as I
import Data.Array.Parallel.Prelude.Double as D



import qualified Prelude



type MMatrix = [:D.Double:]

div = I.div

seqSize = 1::Prelude.Int


mmMult m n  = mmMult' (128::Prelude.Int)   (fromPArrayP m) (fromPArrayP n) 

mmMult':: Prelude.Int -> MMatrix -> MMatrix -> MMatrix
mmMult' order m n = m

{-
-- assumes size is 2^n
mmMult':: Prelude.Int -> MMatrix -> MMatrix -> MMatrix
mmMult' order m n 
  | order I.== (1::Prelude.Int)  = simpleMult m n
  | otherwise           = n
      where
        o2   = order `div` (2::Prelude.Int)
        subs = replicateP 4 (o2 I.* o2)
        mss  = attachShape (mkShape subs) m
        nss  = attachShape (mkShape subs) n
        res  = mapP (Prelude.uncurry (mmMult' o2)) (zipP mss nss)

simpleMult::[:Double:] -> [:Double:] ->[:Double:]
simpleMult m n = zipWithP (*) m n
-}
     {-
foo n = mapP sumP  ((replicateP (4::Prelude.Int) (replicateP (1::Prelude.Int) (10.0))))
-}