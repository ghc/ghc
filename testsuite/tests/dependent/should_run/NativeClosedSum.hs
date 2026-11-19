{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, TypeOperators #-}
module Main (main) where

import Prelude
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeNats (Nat, type (+))
import qualified NativeClosedSumLib

total :: Prelude.sum '[2, 3, 5] :~: 10
total = Refl

other :: NativeClosedSumLib.weighted '[2, 3, 5] :~: 20
other = Refl

contains :: NativeClosedSumLib.containsAny '[2, 3, 5] :~: True
contains = Refl

char :: NativeClosedSumLib.identity 'x' :~: 'x'
char = Refl

-- | A vector whose first parameter counts its cells.
data Vec (n :: Nat) a where
    VNil :: Vec 0 a
    VCons :: a -> Vec n a -> Vec (n + 1) a

-- | The index determines the target of fromIntegral.
vector :: Vec (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) Bool
vector = VCons True (VCons False (VCons True VNil))

-- | An explicit kind can supply the same target.
ascribed :: (Prelude.fromIntegral (Prelude.length '[1, 2, 3]) :: Nat) :~: 3
ascribed = Refl

-- | Pass a negative Int between ordinary calls before converting its magnitude.
signed :: (Prelude.fromIntegral
             (Prelude.abs (Prelude.negate (Prelude.length '[1, 2, 3, 4]))) :: Nat) :~: 4
signed = Refl

-- | Specialize the result of a saturated call on scalar arguments.
positional :: (NativeClosedSumLib.ambiguousPair 18 24 :: Nat) :~: 0
positional = Refl

-- | Specialize an overloaded count of a constructor argument.
constructor :: (NativeClosedSumLib.countOptional ('Just 6) 'True :: Nat) :~: 1
constructor = Refl

main :: IO ()
main = case (total, other, contains, char, vector, ascribed, signed, positional, constructor) of
    (Refl, Refl, Refl, Refl, VCons True (VCons False (VCons True VNil)), Refl, Refl, Refl, Refl) ->
        putStrLn "closed overloaded calls"
