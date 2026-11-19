{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeNats (Nat)
import qualified NativeClosedSumLib as P

-- | Use the monomorphic argument type of the consumer.
unary :: P.keepNatural (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) :~: 3
unary = Refl

-- | Propagate the result kind through the ordinary identity.
parentResult :: (Prelude.id (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) :: Nat) :~: 3
parentResult = Refl

-- | A preceding literal determines the target of the conversion.
siblingLeft :: Prelude.max 2 (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) :~: 3
siblingLeft = Refl

-- | A following literal supplies the same information.
siblingRight :: Prelude.max (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) 2 :~: 3
siblingRight = Refl

-- | Use a constructor literal to infer the other operand kind.
jointLeft :: P.chooseLeft ('Just 6) (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) :~: 6
jointLeft = Refl

-- | Check the same constructor dependency in reverse argument order.
jointRight :: P.chooseRight (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) ('Just 6) :~: 6
jointRight = Refl

-- | Leave the exceptional input unevaluated when the consumer ignores it.
lazy :: P.ignoreNatural (Prelude.fromIntegral (P.bottomSigned 1)) :~: 'True
lazy = Refl

main :: IO ()
main = case (unary, parentResult, siblingLeft, siblingRight, jointLeft, jointRight, lazy) of
  (Refl, Refl, Refl, Refl, Refl, Refl, Refl) -> putStrLn "nested expected kinds"
