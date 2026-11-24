{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
-- | The additive right-fold law from the PLFH Lists chapter.
module BookFoldProof (foldrPlusAppend, checkedSum) where

import BookCongruence (plusAssoc)
import Data.Type.Equality (sym)
import qualified GHC.Internal.List as List
import HaskLib (cong, trans, (:~:)(Refl))
import HaskLib.Nat (Nat(..), plus)

-- | Summing two concatenated lists gives the sum of the separate folds.
foldrPlusAppend :: foreach (xs :: [Nat]) -> foreach (ys :: [Nat])
                -> List.foldr plus Zero (xs ++ ys)
                :~: plus (List.foldr plus Zero xs) (List.foldr plus Zero ys)
foldrPlusAppend [] _ = Refl
foldrPlusAppend (x : xs) ys =
    trans (cong @(plus x) (foldrPlusAppend xs ys))
          (sym (plusAssoc x (List.foldr plus Zero xs) (List.foldr plus Zero ys)))

-- | Use the fold proof before returning the sum of the two lists.
checkedSum :: [Nat] -> [Nat] -> Nat
checkedSum xs ys = case foldrPlusAppend xs ys of
    Refl -> plus (List.foldr plus Zero xs) (List.foldr plus Zero ys)
