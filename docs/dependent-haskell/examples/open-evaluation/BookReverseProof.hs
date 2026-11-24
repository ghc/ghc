{-# LANGUAGE ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
-- | Correctness of list reversal with an accumulator.
module BookReverseProof (shunt, shuntReverse, checkShuntReverse) where

import HaskLib (appendAssoc, (:~:)(Refl))

-- | Append each head to the reversed tail.
rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

-- | Prepend the input elements to the accumulator in reverse order.
shunt :: [a] -> [a] -> [a]
shunt [] ys = ys
shunt (x : xs) ys = shunt xs (x : ys)

-- | The accumulator follows the reversed input in the result.
shuntReverse :: forall a. foreach (xs :: [a]) -> foreach (ys :: [a])
             -> shunt xs ys :~: (rev xs ++ ys)
shuntReverse [] _ = Refl
shuntReverse (x : xs) ys = case shuntReverse xs (x : ys) of
    Refl -> case appendAssoc (rev @a xs) (x : []) ys of
        Refl -> Refl

-- | Check the witness where the equations for Prelude append are available.
checkShuntReverse :: [a] -> [a] -> Bool
checkShuntReverse xs ys = case shuntReverse xs ys of Refl -> True
