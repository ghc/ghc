{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.Num (Num (..)) where

-- For why this file exists
-- See Note [Semigroup stimes cycle] in GHC.Internal.Base

import GHC.Num.Integer (Integer)

infixl 7  *
infixl 6  +, -

class Num a where
    {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}

    (+), (-), (*)       :: a -> a -> a
    negate              :: a -> a
    abs                 :: a -> a
    signum              :: a -> a
    fromInteger         :: Integer -> a

    x - y               = x + negate y
    negate x            = 0 - x
