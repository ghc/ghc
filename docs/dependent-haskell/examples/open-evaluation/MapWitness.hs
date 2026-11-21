{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeOperators #-}
module MapWitness (NegateAll, First(..), Checked(..), checked) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import qualified Prelude as P

type NegateAll xs = P.map P.not xs

cons :: Proxy xs -> NegateAll ('P.True ': xs) :~: ('P.False ': NegateAll xs)
cons _ = Refl

-- | A value identifying the first Boolean in a nonempty list index.
data First (xs :: [P.Bool]) where
  FalseHead :: First ('P.False ': xs)
  TrueHead  :: First ('P.True ': xs)

-- | The equality transports a head from the hidden list to the public index.
data Checked xs where
  Checked :: xs :~: ys -> First ys -> Checked xs

-- | Package the head of the negated singleton list.
checked :: Checked (NegateAll '[ 'P.True])
checked = Checked (cons (Proxy :: Proxy '[])) FalseHead
