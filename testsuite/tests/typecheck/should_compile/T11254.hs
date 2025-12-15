-- /tmp/panic.hs
--
--
--
{-# LANGUAGE TypeFamilies            #-}
--
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module T11254 where

class (Frac (Frac a) ~ Frac a, Fractional (Frac a), ID (Frac a)) => ID a where
  type Frac a
  embed :: a -> Frac a

instance ID Rational where
  type Frac Rational = Int
  embed :: Rational -> Rational
  embed = undefined
