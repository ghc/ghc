{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
import qualified NativeClosedReturnedPairLib as P

instance {-# OVERLAPPING #-} Show (Natural, Natural) where
  show _ = "(2,3)"

division :: Prelude.quotRem 17 5 :~: '(3, 2)
division = Refl

compound :: P.mixed 9 :~: '( '(9, 'True), '[ '\n', 'é', '7', '"', '\\'])
compound = Refl

nested :: P.score (P.mixed 9) :~: 20
nested = Refl

joint :: P.checkEmpty (P.flagged 'x') '[] :~: 11
joint = Refl

dependency :: P.pair 3 :~: '(4, 3)
dependency = Refl

ignoredPair :: P.ignorePair (P.bottomField 1) :~: 7
ignoredPair = Refl

emptySibling :: P.keepSecond (P.firstOf '[]) (P.firstOf '[ 'x']) :~: 'x'
emptySibling = Refl

-- | Infer a compound element kind from the other computed operand.
compoundEmpty :: Prelude.max (Prelude.reverse '[]) (Prelude.reverse '[ 'Just 'x']) :~: '[ 'Just 'x']
compoundEmpty = Refl

main :: IO ()
main = putStrLn (show ((3, 2) :: (Natural, Natural)))
