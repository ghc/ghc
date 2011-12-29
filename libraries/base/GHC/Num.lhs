\begin{code}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Num
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Num' class and the 'Integer' type.
--
-----------------------------------------------------------------------------

-- #hide
module GHC.Num (module GHC.Num, module GHC.Integer) where

import GHC.Base
import GHC.Integer

infixl 7  *
infixl 6  +, -

default ()              -- Double isn't available yet,
                        -- and we shouldn't be using defaults anyway
\end{code}

%*********************************************************
%*                                                      *
\subsection{Standard numeric class}
%*                                                      *
%*********************************************************

\begin{code}
-- | Basic numeric class.
--
-- Minimal complete definition: all except 'negate' or @(-)@
class  Num a  where
    (+), (-), (*)       :: a -> a -> a
    -- | Unary negation.
    negate              :: a -> a
    -- | Absolute value.
    abs                 :: a -> a
    -- | Sign of a number.
    -- The functions 'abs' and 'signum' should satisfy the law:
    --
    -- > abs x * signum x == x
    --
    -- For real numbers, the 'signum' is either @-1@ (negative), @0@ (zero)
    -- or @1@ (positive).
    signum              :: a -> a
    -- | Conversion from an 'Integer'.
    -- An integer literal represents the application of the function
    -- 'fromInteger' to the appropriate value of type 'Integer',
    -- so such literals have type @('Num' a) => a@.
    fromInteger         :: Integer -> a

    {-# INLINE (-) #-}
    {-# INLINE negate #-}
    x - y               = x + negate y
    negate x            = 0 - x

-- | the same as @'flip' ('-')@.
--
-- Because @-@ is treated specially in the Haskell grammar,
-- @(-@ /e/@)@ is not a section, but an application of prefix negation.
-- However, @('subtract'@ /exp/@)@ is equivalent to the disallowed section.
{-# INLINE subtract #-}
subtract :: (Num a) => a -> a -> a
subtract x y = y - x
\end{code}


%*********************************************************
%*                                                      *
\subsection{Instances for @Int@}
%*                                                      *
%*********************************************************

\begin{code}
instance  Num Int  where
    (+)    = plusInt
    (-)    = minusInt
    negate = negateInt
    (*)    = timesInt
    abs n  = if n `geInt` 0 then n else negateInt n

    signum n | n `ltInt` 0 = negateInt 1
             | n `eqInt` 0 = 0
             | otherwise   = 1

    {-# INLINE fromInteger #-}	 -- Just to be sure!
    fromInteger i = I# (integerToInt i)

quotRemInt :: Int -> Int -> (Int, Int)
quotRemInt a@(I# _) b@(I# _) = (a `quotInt` b, a `remInt` b)
    -- OK, so I made it a little stricter.  Shoot me.  (WDP 94/10)

divModInt ::  Int -> Int -> (Int, Int)
divModInt x@(I# _) y@(I# _) = (x `divInt` y, x `modInt` y)
    -- Stricter.  Sorry if you don't like it.  (WDP 94/10)
\end{code}

%*********************************************************
%*                                                      *
\subsection{The @Integer@ instances for @Num@}
%*                                                      *
%*********************************************************

\begin{code}
instance  Num Integer  where
    (+) = plusInteger
    (-) = minusInteger
    (*) = timesInteger
    negate         = negateInteger
    fromInteger x  =  x

    abs = absInteger
    signum = signumInteger
\end{code}

