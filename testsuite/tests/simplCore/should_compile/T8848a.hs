module T8848a where

f :: Ord a => b -> a -> a
f y x = x

{-# SPECIALISE f :: b -> [Int] -> [Int] #-}

{- Specialised badly:

"SPEC Spec.f" [ALWAYS]
    forall (@ b_aX7).
      Spec.f @ b_aX7
             @ [GHC.Types.Int]
             (GHC.Classes.$fOrd[]
                @ GHC.Types.Int
                (GHC.Classes.$fEq[] @ GHC.Types.Int GHC.Classes.$fEqInt)
                GHC.Classes.$fOrdInt)
      = Spec.f_$sf @ b_aX7
-}