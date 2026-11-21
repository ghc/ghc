{-# LANGUAGE GHC2024, NoExperimentalDependentTypes #-}
module HomogeneousSelectorRole where

import Data.Coerce (Coercible)

type role Premise representational
data Premise (a :: k) = Premise

type role Result nominal
data Result (a :: k) = Result

bad :: forall k l (a :: k) (b :: l).
       Coercible (Premise a) (Premise b) => Result a -> Result b
bad x = x
