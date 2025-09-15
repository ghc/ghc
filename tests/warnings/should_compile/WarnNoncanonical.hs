module WarnNoncanonical where

import qualified Data.Semigroup as Semi

-- -fwarn-noncanonical-monoid-instances
newtype S = S Int

instance Semi.Semigroup S where
  (<>) = mappend

instance Monoid S where
  S a `mappend` S b = S (a+b)
  mempty = S 0

newtype M a = M a

instance Functor M where
  fmap = undefined

instance Applicative M where
  liftA2 = undefined
  pure = return
  (*>) = (>>)

instance Monad M where
  return = undefined
  (>>=) = undefined
  (>>) = undefined
