{-# LANGUAGE KindSignatures #-}

module WCompatWarningsOnOff where

import qualified Data.Semigroup as Semi

(<>) = undefined -- Semigroup warnings

-- -fwarn-noncanonical-monoid-instances
newtype S = S Int

instance Semi.Semigroup S where
  (<>) = mappend

instance Monoid S where
  S a `mappend` S b = S (a+b)
  mempty = S 0

-- -fwarn-star-is-type
b :: (Bool :: *)
b = True
