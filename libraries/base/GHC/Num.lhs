\begin{code}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
-- We believe we could deorphan this module, by moving lots of things
-- around, but we haven't got there yet:
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

#include "MachDeps.h"
#if SIZEOF_HSWORD == 4
#define DIGITS       9
#define BASE         1000000000
#elif SIZEOF_HSWORD == 8
#define DIGITS       18
#define BASE         1000000000000000000
#else
#error Please define DIGITS and BASE
-- DIGITS should be the largest integer such that
--     10^DIGITS < 2^(SIZEOF_HSWORD * 8 - 1)
-- BASE should be 10^DIGITS. Note that ^ is not available yet.
#endif

-- #hide
module GHC.Num (module GHC.Num, module GHC.Integer) where

import GHC.Base
import GHC.Enum
import GHC.Show
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
class  (Eq a, Show a) => Num a  where
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

    fromInteger i = I# (toInt# i)

quotRemInt :: Int -> Int -> (Int, Int)
quotRemInt a@(I# _) b@(I# _) = (a `quotInt` b, a `remInt` b)
    -- OK, so I made it a little stricter.  Shoot me.  (WDP 94/10)

divModInt ::  Int -> Int -> (Int, Int)
divModInt x@(I# _) y@(I# _) = (x `divInt` y, x `modInt` y)
    -- Stricter.  Sorry if you don't like it.  (WDP 94/10)
\end{code}

%*********************************************************
%*                                                      *
\subsection{The @Integer@ instances for @Show@}
%*                                                      *
%*********************************************************

\begin{code}
instance Show Integer where
    showsPrec p n r
        | p > 6 && n < 0 = '(' : integerToString n (')' : r)
        -- Minor point: testing p first gives better code
        -- in the not-uncommon case where the p argument
        -- is a constant
        | otherwise = integerToString n r
    showList = showList__ (showsPrec 0)

-- Divide an conquer implementation of string conversion
integerToString :: Integer -> String -> String
integerToString n0 cs0
    | n0 < 0    = '-' : integerToString' (- n0) cs0
    | otherwise = integerToString' n0 cs0
    where
    integerToString' :: Integer -> String -> String
    integerToString' n cs
        | n < BASE  = jhead (fromInteger n) cs
        | otherwise = jprinth (jsplitf (BASE*BASE) n) cs

    -- Split n into digits in base p. We first split n into digits
    -- in base p*p and then split each of these digits into two.
    -- Note that the first 'digit' modulo p*p may have a leading zero
    -- in base p that we need to drop - this is what jsplith takes care of.
    -- jsplitb the handles the remaining digits.
    jsplitf :: Integer -> Integer -> [Integer]
    jsplitf p n
        | p > n     = [n]
        | otherwise = jsplith p (jsplitf (p*p) n)

    jsplith :: Integer -> [Integer] -> [Integer]
    jsplith p (n:ns) =
        case n `quotRemInteger` p of
        (# q, r #) ->
            if q > 0 then q : r : jsplitb p ns
                     else     r : jsplitb p ns
    jsplith _ [] = error "jsplith: []"

    jsplitb :: Integer -> [Integer] -> [Integer]
    jsplitb _ []     = []
    jsplitb p (n:ns) = case n `quotRemInteger` p of
                       (# q, r #) ->
                           q : r : jsplitb p ns

    -- Convert a number that has been split into digits in base BASE^2
    -- this includes a last splitting step and then conversion of digits
    -- that all fit into a machine word.
    jprinth :: [Integer] -> String -> String
    jprinth (n:ns) cs =
        case n `quotRemInteger` BASE of
        (# q', r' #) ->
            let q = fromInteger q'
                r = fromInteger r'
            in if q > 0 then jhead q $ jblock r $ jprintb ns cs
                        else jhead r $ jprintb ns cs
    jprinth [] _ = error "jprinth []"

    jprintb :: [Integer] -> String -> String
    jprintb []     cs = cs
    jprintb (n:ns) cs = case n `quotRemInteger` BASE of
                        (# q', r' #) ->
                            let q = fromInteger q'
                                r = fromInteger r'
                            in jblock q $ jblock r $ jprintb ns cs

    -- Convert an integer that fits into a machine word. Again, we have two
    -- functions, one that drops leading zeros (jhead) and one that doesn't
    -- (jblock)
    jhead :: Int -> String -> String
    jhead n cs
        | n < 10    = case unsafeChr (ord '0' + n) of
            c@(C# _) -> c : cs
        | otherwise = case unsafeChr (ord '0' + r) of
            c@(C# _) -> jhead q (c : cs)
        where
        (q, r) = n `quotRemInt` 10

    jblock = jblock' {- ' -} DIGITS

    jblock' :: Int -> Int -> String -> String
    jblock' d n cs
        | d == 1    = case unsafeChr (ord '0' + n) of
             c@(C# _) -> c : cs
        | otherwise = case unsafeChr (ord '0' + r) of
             c@(C# _) -> jblock' (d - 1) q (c : cs)
        where
        (q, r) = n `quotRemInt` 10
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


%*********************************************************
%*                                                      *
\subsection{The @Integer@ instance for @Enum@}
%*                                                      *
%*********************************************************

\begin{code}
instance  Enum Integer  where
    succ x               = x + 1
    pred x               = x - 1
    toEnum (I# n)        = smallInteger n
    fromEnum n           = I# (toInt# n)

    {-# INLINE enumFrom #-}
    {-# INLINE enumFromThen #-}
    {-# INLINE enumFromTo #-}
    {-# INLINE enumFromThenTo #-}
    enumFrom x             = enumDeltaInteger  x 1
    enumFromThen x y       = enumDeltaInteger  x (y-x)
    enumFromTo x lim       = enumDeltaToInteger x 1     lim
    enumFromThenTo x y lim = enumDeltaToInteger x (y-x) lim

{-# RULES
"enumDeltaInteger"      [~1] forall x y.  enumDeltaInteger x y     = build (\c _ -> enumDeltaIntegerFB c x y)
"efdtInteger"           [~1] forall x y l.enumDeltaToInteger x y l = build (\c n -> enumDeltaToIntegerFB c n x y l)
"enumDeltaInteger"      [1] enumDeltaIntegerFB   (:)    = enumDeltaInteger
"enumDeltaToInteger"    [1] enumDeltaToIntegerFB (:) [] = enumDeltaToInteger
 #-}

enumDeltaIntegerFB :: (Integer -> b -> b) -> Integer -> Integer -> b
enumDeltaIntegerFB c x d = x `seq` (x `c` enumDeltaIntegerFB c (x+d) d)

enumDeltaInteger :: Integer -> Integer -> [Integer]
enumDeltaInteger x d = x `seq` (x : enumDeltaInteger (x+d) d)
-- strict accumulator, so
--     head (drop 1000000 [1 .. ]
-- works

{-# NOINLINE [0] enumDeltaToIntegerFB #-}
-- Don't inline this until RULE "enumDeltaToInteger" has had a chance to fire
enumDeltaToIntegerFB :: (Integer -> a -> a) -> a
                     -> Integer -> Integer -> Integer -> a
enumDeltaToIntegerFB c n x delta lim
  | delta >= 0 = up_fb c n x delta lim
  | otherwise  = dn_fb c n x delta lim

enumDeltaToInteger :: Integer -> Integer -> Integer -> [Integer]
enumDeltaToInteger x delta lim
  | delta >= 0 = up_list x delta lim
  | otherwise  = dn_list x delta lim

up_fb :: (Integer -> a -> a) -> a -> Integer -> Integer -> Integer -> a
up_fb c n x0 delta lim = go (x0 :: Integer)
                      where
                        go x | x > lim   = n
                             | otherwise = x `c` go (x+delta)
dn_fb :: (Integer -> a -> a) -> a -> Integer -> Integer -> Integer -> a
dn_fb c n x0 delta lim = go (x0 :: Integer)
                      where
                        go x | x < lim   = n
                             | otherwise = x `c` go (x+delta)

up_list :: Integer -> Integer -> Integer -> [Integer]
up_list x0 delta lim = go (x0 :: Integer)
                    where
                        go x | x > lim   = []
                             | otherwise = x : go (x+delta)
dn_list :: Integer -> Integer -> Integer -> [Integer]
dn_list x0 delta lim = go (x0 :: Integer)
                    where
                        go x | x < lim   = []
                             | otherwise = x : go (x+delta)
\end{code}

