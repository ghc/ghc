
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- XXX -fno-warn-unused-imports needed for the GHC.Tuple import below. Sigh.
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Classes
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic classes.
--
-----------------------------------------------------------------------------

module GHC.Classes where

import GHC.Bool
-- GHC.Magic is used in some derived instances
import GHC.Magic ()
import GHC.Ordering
import GHC.Prim
import GHC.Tuple
import GHC.Types
import GHC.Unit

infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||

default ()              -- Double isn't available yet

-- | The 'Eq' class defines equality ('==') and inequality ('/=').
-- All the basic datatypes exported by the "Prelude" are instances of 'Eq',
-- and 'Eq' may be derived for any datatype whose constituents are also
-- instances of 'Eq'.
--
-- Minimal complete definition: either '==' or '/='.
--
class  Eq a  where
    (==), (/=)           :: a -> a -> Bool

    {-# INLINE (/=) #-}
    {-# INLINE (==) #-}
    x /= y               = not (x == y)
    x == y               = not (x /= y)

deriving instance Eq ()
deriving instance (Eq  a, Eq  b) => Eq  (a, b)
deriving instance (Eq  a, Eq  b, Eq  c) => Eq  (a, b, c)
deriving instance (Eq  a, Eq  b, Eq  c, Eq  d) => Eq  (a, b, c, d)
deriving instance (Eq  a, Eq  b, Eq  c, Eq  d, Eq  e) => Eq  (a, b, c, d, e)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f)
               => Eq (a, b, c, d, e, f)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g)
               => Eq (a, b, c, d, e, f, g)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h)
               => Eq (a, b, c, d, e, f, g, h)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i)
               => Eq (a, b, c, d, e, f, g, h, i)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j)
               => Eq (a, b, c, d, e, f, g, h, i, j)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k)
               => Eq (a, b, c, d, e, f, g, h, i, j, k)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance (Eq a) => Eq [a] where
    {-# SPECIALISE instance Eq [Char] #-}
    []     == []     = True
    (x:xs) == (y:ys) = x == y && xs == ys
    _xs    == _ys    = False

-- XXX This doesn't work:
-- deriving instance Eq Bool
-- <wired into compiler>:
--     Illegal binding of built-in syntax: con2tag_Bool#
instance Eq Bool where
    True  == True  = True
    False == False = True
    _     == _     = False

-- XXX This doesn't work:
-- deriving instance Eq Ordering
-- Illegal binding of built-in syntax: con2tag_Ordering#
instance Eq Ordering where
    EQ == EQ = True
    LT == LT = True
    GT == GT = True
    _  == _  = False

instance Eq Char where
    (C# c1) == (C# c2) = c1 `eqChar#` c2
    (C# c1) /= (C# c2) = c1 `neChar#` c2

-- | The 'Ord' class is used for totally ordered datatypes.
--
-- Instances of 'Ord' can be derived for any user-defined
-- datatype whose constituent types are in 'Ord'.  The declared order
-- of the constructors in the data declaration determines the ordering
-- in derived 'Ord' instances.  The 'Ordering' datatype allows a single
-- comparison to determine the precise ordering of two objects.
--
-- Minimal complete definition: either 'compare' or '<='.
-- Using 'compare' can be more efficient for complex types.
--
class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y = if x == y then EQ
                  -- NB: must be '<=' not '<' to validate the
                  -- above claim about the minimal things that
                  -- can be defined for an instance of Ord:
                  else if x <= y then LT
                  else GT

    x <  y = case compare x y of { LT -> True;  _ -> False }
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >  y = case compare x y of { GT -> True;  _ -> False }
    x >= y = case compare x y of { LT -> False; _ -> True }

        -- These two default methods use '<=' rather than 'compare'
        -- because the latter is often more expensive
    max x y = if x <= y then y else x
    min x y = if x <= y then x else y

-- XXX Deriving this doesn't work:
-- ghc-stage1: panic! (the 'impossible' happened)
--   (GHC version 6.13.20091123 for x86_64-unknown-linux):
--         TcGenDeriv:mk_FunBind
instance Ord () where
    () <= () = True
    () <  () = False
    () >= () = True
    () >  () = False
    max () () = ()
    min () () = ()
    compare () () = EQ

deriving instance (Ord a, Ord b) => Ord (a, b)
deriving instance (Ord a, Ord b, Ord c) => Ord (a, b, c)
deriving instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f)
               => Ord (a, b, c, d, e, f)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g)
               => Ord (a, b, c, d, e, f, g)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h)
               => Ord (a, b, c, d, e, f, g, h)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i)
               => Ord (a, b, c, d, e, f, g, h, i)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j)
               => Ord (a, b, c, d, e, f, g, h, i, j)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k)
               => Ord (a, b, c, d, e, f, g, h, i, j, k)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance (Ord a) => Ord [a] where
    {-# SPECIALISE instance Ord [Char] #-}
    compare []     []     = EQ
    compare []     (_:_)  = LT
    compare (_:_)  []     = GT
    compare (x:xs) (y:ys) = case compare x y of
                                EQ    -> compare xs ys
                                other -> other

-- XXX This doesn't work:
-- deriving instance Ord Bool
-- <wired into compiler>:
--     Illegal binding of built-in syntax: con2tag_Bool#
instance Ord Bool where
    compare False True  = LT
    compare True  False = GT
    compare _     _     = EQ

-- XXX This doesn't work:
-- deriving instance Ord Ordering
-- Illegal binding of built-in syntax: con2tag_Ordering#
instance Ord Ordering where
    LT <= _  = True
    _  <= LT = False
    EQ <= _  = True
    _  <= EQ = False
    GT <= GT = True

-- We don't use deriving for Ord Char, because for Ord the derived
-- instance defines only compare, which takes two primops.  Then
-- '>' uses compare, and therefore takes two primops instead of one.
instance Ord Char where
    (C# c1) >  (C# c2) = c1 `gtChar#` c2
    (C# c1) >= (C# c2) = c1 `geChar#` c2
    (C# c1) <= (C# c2) = c1 `leChar#` c2
    (C# c1) <  (C# c2) = c1 `ltChar#` c2

-- OK, so they're technically not part of a class...:

-- Boolean functions

-- | Boolean \"and\"
(&&)                    :: Bool -> Bool -> Bool
True  && x              =  x
False && _              =  False

-- | Boolean \"or\"
(||)                    :: Bool -> Bool -> Bool
True  || _              =  True
False || x              =  x

-- | Boolean \"not\"
not                     :: Bool -> Bool
not True                =  False
not False               =  True

