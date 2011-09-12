\begin{code}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Num (Num(..)) where

import GHC.Base
import GHC.Show
import GHC.Integer

infixl 7  *
infixl 6  +, -

default ()

class  (Eq a, Show a) => Num a  where
    (+), (-), (*)       :: a -> a -> a
    (+) = (+)
    (-) = (-)
    (*) = (*)
    negate              :: a -> a
    negate = negate
    abs                 :: a -> a
    abs = abs
    signum              :: a -> a
    signum = signum
    fromInteger         :: Integer -> a
    fromInteger = fromInteger
\end{code}

