{-# LANGUAGE FlexibleContexts, TypeSynonymInstances,
             FlexibleInstances, TypeFamilies,
             UndecidableSuperClasses #-}

module T10318 where

import Data.Kind (Type)

-- | Product of non-zero elements always non-zero.
-- Every integral domain has a field of fractions.
-- The field of fractions of any field is itself.
class (Frac (Frac a) ~ Frac a, Fractional (Frac a), IntegralDomain (Frac a))
  => IntegralDomain a where
  type Frac a :: Type
  embed :: a -> Frac a

instance IntegralDomain Integer where
  type Frac Integer = Rational
  embed = fromInteger

instance IntegralDomain Rational where
  type Frac Rational = Rational
  embed = id

g :: IntegralDomain a => a -> a
g x = g x

h :: a -> Frac a
h x = h x

-- This is the test function

f :: IntegralDomain a => a -> Frac a
f x = g (h (h x))
  -- Given: IntegralDomain (Frac a)
  -- Wanted: IntegralDomain (Frac (Frac a))

