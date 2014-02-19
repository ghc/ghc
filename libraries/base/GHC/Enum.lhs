\begin{code}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, BangPatterns, MagicHash #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Enum
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- The 'Enum' and 'Bounded' classes.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module GHC.Enum(
        Bounded(..), Enum(..),
        boundedEnumFrom, boundedEnumFromThen,
        toEnumError, fromEnumError, succError, predError,

        -- Instances for Bounded and Enum: (), Char, Int

   ) where

import GHC.Base
import GHC.Char
import GHC.Integer
import GHC.Num
import GHC.Show
default ()              -- Double isn't available yet
\end{code}


%*********************************************************
%*                                                      *
\subsection{Class declarations}
%*                                                      *
%*********************************************************

\begin{code}
-- | The 'Bounded' class is used to name the upper and lower limits of a
-- type.  'Ord' is not a superclass of 'Bounded' since types that are not
-- totally ordered may also have upper and lower bounds.
--
-- The 'Bounded' class may be derived for any enumeration type;
-- 'minBound' is the first constructor listed in the @data@ declaration
-- and 'maxBound' is the last.
-- 'Bounded' may also be derived for single-constructor datatypes whose
-- constituent types are in 'Bounded'.

class  Bounded a  where
    minBound, maxBound :: a

-- | Class 'Enum' defines operations on sequentially ordered types.
--
-- The @enumFrom@... methods are used in Haskell's translation of
-- arithmetic sequences.
--
-- Instances of 'Enum' may be derived for any enumeration type (types
-- whose constructors have no fields).  The nullary constructors are
-- assumed to be numbered left-to-right by 'fromEnum' from @0@ through @n-1@.
-- See Chapter 10 of the /Haskell Report/ for more details.
--
-- For any type that is an instance of class 'Bounded' as well as 'Enum',
-- the following should hold:
--
-- * The calls @'succ' 'maxBound'@ and @'pred' 'minBound'@ should result in
--   a runtime error.
--
-- * 'fromEnum' and 'toEnum' should give a runtime error if the
--   result value is not representable in the result type.
--   For example, @'toEnum' 7 :: 'Bool'@ is an error.
--
-- * 'enumFrom' and 'enumFromThen' should be defined with an implicit bound,
--   thus:
--
-- >    enumFrom     x   = enumFromTo     x maxBound
-- >    enumFromThen x y = enumFromThenTo x y bound
-- >      where
-- >        bound | fromEnum y >= fromEnum x = maxBound
-- >              | otherwise                = minBound
--
class  Enum a   where
    -- | the successor of a value.  For numeric types, 'succ' adds 1.
    succ                :: a -> a
    -- | the predecessor of a value.  For numeric types, 'pred' subtracts 1.
    pred                :: a -> a
    -- | Convert from an 'Int'.
    toEnum              :: Int -> a
    -- | Convert to an 'Int'.
    -- It is implementation-dependent what 'fromEnum' returns when
    -- applied to a value that is too large to fit in an 'Int'.
    fromEnum            :: a -> Int

    -- | Used in Haskell's translation of @[n..]@.
    enumFrom            :: a -> [a]
    -- | Used in Haskell's translation of @[n,n'..]@.
    enumFromThen        :: a -> a -> [a]
    -- | Used in Haskell's translation of @[n..m]@.
    enumFromTo          :: a -> a -> [a]
    -- | Used in Haskell's translation of @[n,n'..m]@.
    enumFromThenTo      :: a -> a -> a -> [a]

    succ                   = toEnum . (+ 1)  . fromEnum
    pred                   = toEnum . (subtract 1) . fromEnum
    enumFrom x             = map toEnum [fromEnum x ..]
    enumFromThen x y       = map toEnum [fromEnum x, fromEnum y ..]
    enumFromTo x y         = map toEnum [fromEnum x .. fromEnum y]
    enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

-- Default methods for bounded enumerations
boundedEnumFrom :: (Enum a, Bounded a) => a -> [a]
boundedEnumFrom n = map toEnum [fromEnum n .. fromEnum (maxBound `asTypeOf` n)]

boundedEnumFromThen :: (Enum a, Bounded a) => a -> a -> [a]
boundedEnumFromThen n1 n2
  | i_n2 >= i_n1  = map toEnum [i_n1, i_n2 .. fromEnum (maxBound `asTypeOf` n1)]
  | otherwise     = map toEnum [i_n1, i_n2 .. fromEnum (minBound `asTypeOf` n1)]
  where
    i_n1 = fromEnum n1
    i_n2 = fromEnum n2
\end{code}

\begin{code}
------------------------------------------------------------------------
-- Helper functions
------------------------------------------------------------------------

{-# NOINLINE toEnumError #-}
toEnumError :: (Show a) => String -> Int -> (a,a) -> b
toEnumError inst_ty i bnds =
    error $ "Enum.toEnum{" ++ inst_ty ++ "}: tag (" ++
            show i ++
            ") is outside of bounds " ++
            show bnds

{-# NOINLINE fromEnumError #-}
fromEnumError :: (Show a) => String -> a -> b
fromEnumError inst_ty x =
    error $ "Enum.fromEnum{" ++ inst_ty ++ "}: value (" ++
            show x ++
            ") is outside of Int's bounds " ++
            show (minBound::Int, maxBound::Int)

{-# NOINLINE succError #-}
succError :: String -> a
succError inst_ty =
    error $ "Enum.succ{" ++ inst_ty ++ "}: tried to take `succ' of maxBound"

{-# NOINLINE predError #-}
predError :: String -> a
predError inst_ty =
    error $ "Enum.pred{" ++ inst_ty ++ "}: tried to take `pred' of minBound"
\end{code}


%*********************************************************
%*                                                      *
\subsection{Tuples}
%*                                                      *
%*********************************************************

\begin{code}
instance Bounded () where
    minBound = ()
    maxBound = ()

instance Enum () where
    succ _      = error "Prelude.Enum.().succ: bad argument"
    pred _      = error "Prelude.Enum.().pred: bad argument"

    toEnum x | x == 0    = ()
             | otherwise = error "Prelude.Enum.().toEnum: bad argument"

    fromEnum () = 0
    enumFrom ()         = [()]
    enumFromThen () ()  = let many = ():many in many
    enumFromTo () ()    = [()]
    enumFromThenTo () () () = let many = ():many in many
\end{code}

\begin{code}
-- Report requires instances up to 15
instance (Bounded a, Bounded b) => Bounded (a,b) where
   minBound = (minBound, minBound)
   maxBound = (maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c) => Bounded (a,b,c) where
   minBound = (minBound, minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d) => Bounded (a,b,c,d) where
   minBound = (minBound, minBound, minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e) => Bounded (a,b,c,d,e) where
   minBound = (minBound, minBound, minBound, minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f)
        => Bounded (a,b,c,d,e,f) where
   minBound = (minBound, minBound, minBound, minBound, minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g)
        => Bounded (a,b,c,d,e,f,g) where
   minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
          Bounded h)
        => Bounded (a,b,c,d,e,f,g,h) where
   minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
          Bounded h, Bounded i)
        => Bounded (a,b,c,d,e,f,g,h,i) where
   minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound,
               minBound)
   maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound,
               maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
          Bounded h, Bounded i, Bounded j)
        => Bounded (a,b,c,d,e,f,g,h,i,j) where
   minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound,
               minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound,
               maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
          Bounded h, Bounded i, Bounded j, Bounded k)
        => Bounded (a,b,c,d,e,f,g,h,i,j,k) where
   minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound,
               minBound, minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound,
               maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
          Bounded h, Bounded i, Bounded j, Bounded k, Bounded l)
        => Bounded (a,b,c,d,e,f,g,h,i,j,k,l) where
   minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound,
               minBound, minBound, minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound,
               maxBound, maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
          Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m)
        => Bounded (a,b,c,d,e,f,g,h,i,j,k,l,m) where
   minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound,
               minBound, minBound, minBound, minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound,
               maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
          Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m, Bounded n)
        => Bounded (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
   minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound,
               minBound, minBound, minBound, minBound, minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound,
               maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
          Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m, Bounded n, Bounded o)
        => Bounded (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
   minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound,
               minBound, minBound, minBound, minBound, minBound, minBound, minBound)
   maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound,
               maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)
\end{code}


%*********************************************************
%*                                                      *
\subsection{Type @Bool@}
%*                                                      *
%*********************************************************

\begin{code}
instance Bounded Bool where
  minBound = False
  maxBound = True

instance Enum Bool where
  succ False = True
  succ True  = error "Prelude.Enum.Bool.succ: bad argument"

  pred True  = False
  pred False  = error "Prelude.Enum.Bool.pred: bad argument"

  toEnum n | n == 0    = False
           | n == 1    = True
           | otherwise = error "Prelude.Enum.Bool.toEnum: bad argument"

  fromEnum False = 0
  fromEnum True  = 1

  -- Use defaults for the rest
  enumFrom     = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
\end{code}

%*********************************************************
%*                                                      *
\subsection{Type @Ordering@}
%*                                                      *
%*********************************************************

\begin{code}
instance Bounded Ordering where
  minBound = LT
  maxBound = GT

instance Enum Ordering where
  succ LT = EQ
  succ EQ = GT
  succ GT = error "Prelude.Enum.Ordering.succ: bad argument"

  pred GT = EQ
  pred EQ = LT
  pred LT = error "Prelude.Enum.Ordering.pred: bad argument"

  toEnum n | n == 0 = LT
           | n == 1 = EQ
           | n == 2 = GT
  toEnum _ = error "Prelude.Enum.Ordering.toEnum: bad argument"

  fromEnum LT = 0
  fromEnum EQ = 1
  fromEnum GT = 2

  -- Use defaults for the rest
  enumFrom     = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
\end{code}

%*********************************************************
%*                                                      *
\subsection{Type @Char@}
%*                                                      *
%*********************************************************

\begin{code}
instance  Bounded Char  where
    minBound =  '\0'
    maxBound =  '\x10FFFF'

instance  Enum Char  where
    succ (C# c#)
       | isTrue# (ord# c# /=# 0x10FFFF#) = C# (chr# (ord# c# +# 1#))
       | otherwise             = error ("Prelude.Enum.Char.succ: bad argument")
    pred (C# c#)
       | isTrue# (ord# c# /=# 0#) = C# (chr# (ord# c# -# 1#))
       | otherwise                = error ("Prelude.Enum.Char.pred: bad argument")

    toEnum   = chr
    fromEnum = ord

    {-# INLINE enumFrom #-}
    enumFrom (C# x) = eftChar (ord# x) 0x10FFFF#
        -- Blarg: technically I guess enumFrom isn't strict!

    {-# INLINE enumFromTo #-}
    enumFromTo (C# x) (C# y) = eftChar (ord# x) (ord# y)

    {-# INLINE enumFromThen #-}
    enumFromThen (C# x1) (C# x2) = efdChar (ord# x1) (ord# x2)

    {-# INLINE enumFromThenTo #-}
    enumFromThenTo (C# x1) (C# x2) (C# y) = efdtChar (ord# x1) (ord# x2) (ord# y)

{-# RULES
"eftChar"       [~1] forall x y.        eftChar x y       = build (\c n -> eftCharFB c n x y)
"efdChar"       [~1] forall x1 x2.      efdChar x1 x2     = build (\ c n -> efdCharFB c n x1 x2)
"efdtChar"      [~1] forall x1 x2 l.    efdtChar x1 x2 l  = build (\ c n -> efdtCharFB c n x1 x2 l)
"eftCharList"   [1]  eftCharFB  (:) [] = eftChar
"efdCharList"   [1]  efdCharFB  (:) [] = efdChar
"efdtCharList"  [1]  efdtCharFB (:) [] = efdtChar
 #-}


-- We can do better than for Ints because we don't
-- have hassles about arithmetic overflow at maxBound
{-# INLINE [0] eftCharFB #-}
eftCharFB :: (Char -> a -> a) -> a -> Int# -> Int# -> a
eftCharFB c n x0 y = go x0
                 where
                    go x | isTrue# (x ># y) = n
                         | otherwise        = C# (chr# x) `c` go (x +# 1#)

{-# NOINLINE [1] eftChar #-}
eftChar :: Int# -> Int# -> String
eftChar x y | isTrue# (x ># y ) = []
            | otherwise         = C# (chr# x) : eftChar (x +# 1#) y


-- For enumFromThenTo we give up on inlining
{-# NOINLINE [0] efdCharFB #-}
efdCharFB :: (Char -> a -> a) -> a -> Int# -> Int# -> a
efdCharFB c n x1 x2
  | isTrue# (delta >=# 0#) = go_up_char_fb c n x1 delta 0x10FFFF#
  | otherwise              = go_dn_char_fb c n x1 delta 0#
  where
    !delta = x2 -# x1

{-# NOINLINE [1] efdChar #-}
efdChar :: Int# -> Int# -> String
efdChar x1 x2
  | isTrue# (delta >=# 0#) = go_up_char_list x1 delta 0x10FFFF#
  | otherwise              = go_dn_char_list x1 delta 0#
  where
    !delta = x2 -# x1

{-# NOINLINE [0] efdtCharFB #-}
efdtCharFB :: (Char -> a -> a) -> a -> Int# -> Int# -> Int# -> a
efdtCharFB c n x1 x2 lim
  | isTrue# (delta >=# 0#) = go_up_char_fb c n x1 delta lim
  | otherwise              = go_dn_char_fb c n x1 delta lim
  where
    !delta = x2 -# x1

{-# NOINLINE [1] efdtChar #-}
efdtChar :: Int# -> Int# -> Int# -> String
efdtChar x1 x2 lim
  | isTrue# (delta >=# 0#) = go_up_char_list x1 delta lim
  | otherwise              = go_dn_char_list x1 delta lim
  where
    !delta = x2 -# x1

go_up_char_fb :: (Char -> a -> a) -> a -> Int# -> Int# -> Int# -> a
go_up_char_fb c n x0 delta lim
  = go_up x0
  where
    go_up x | isTrue# (x ># lim) = n
            | otherwise          = C# (chr# x) `c` go_up (x +# delta)

go_dn_char_fb :: (Char -> a -> a) -> a -> Int# -> Int# -> Int# -> a
go_dn_char_fb c n x0 delta lim
  = go_dn x0
  where
    go_dn x | isTrue# (x <# lim) = n
            | otherwise          = C# (chr# x) `c` go_dn (x +# delta)

go_up_char_list :: Int# -> Int# -> Int# -> String
go_up_char_list x0 delta lim
  = go_up x0
  where
    go_up x | isTrue# (x ># lim) = []
            | otherwise          = C# (chr# x) : go_up (x +# delta)

go_dn_char_list :: Int# -> Int# -> Int# -> String
go_dn_char_list x0 delta lim
  = go_dn x0
  where
    go_dn x | isTrue# (x <# lim) = []
            | otherwise          = C# (chr# x) : go_dn (x +# delta)
\end{code}


%*********************************************************
%*                                                      *
\subsection{Type @Int@}
%*                                                      *
%*********************************************************

Be careful about these instances.
        (a) remember that you have to count down as well as up e.g. [13,12..0]
        (b) be careful of Int overflow
        (c) remember that Int is bounded, so [1..] terminates at maxInt

\begin{code}
instance  Bounded Int where
    minBound =  minInt
    maxBound =  maxInt

instance  Enum Int  where
    succ x
       | x == maxBound  = error "Prelude.Enum.succ{Int}: tried to take `succ' of maxBound"
       | otherwise      = x + 1
    pred x
       | x == minBound  = error "Prelude.Enum.pred{Int}: tried to take `pred' of minBound"
       | otherwise      = x - 1

    toEnum   x = x
    fromEnum x = x

    {-# INLINE enumFrom #-}
    enumFrom (I# x) = eftInt x maxInt#
        where !(I# maxInt#) = maxInt
        -- Blarg: technically I guess enumFrom isn't strict!

    {-# INLINE enumFromTo #-}
    enumFromTo (I# x) (I# y) = eftInt x y

    {-# INLINE enumFromThen #-}
    enumFromThen (I# x1) (I# x2) = efdInt x1 x2

    {-# INLINE enumFromThenTo #-}
    enumFromThenTo (I# x1) (I# x2) (I# y) = efdtInt x1 x2 y


-----------------------------------------------------
-- eftInt and eftIntFB deal with [a..b], which is the
-- most common form, so we take a lot of care
-- In particular, we have rules for deforestation

{-# RULES
"eftInt"        [~1] forall x y. eftInt x y = build (\ c n -> eftIntFB c n x y)
"eftIntList"    [1] eftIntFB  (:) [] = eftInt
 #-}

{-# NOINLINE [1] eftInt #-}
eftInt :: Int# -> Int# -> [Int]
-- [x1..x2]
eftInt x0 y | isTrue# (x0 ># y) = []
            | otherwise         = go x0
               where
                 go x = I# x : if isTrue# (x ==# y)
                               then []
                               else go (x +# 1#)

{-# INLINE [0] eftIntFB #-}
eftIntFB :: (Int -> r -> r) -> r -> Int# -> Int# -> r
eftIntFB c n x0 y | isTrue# (x0 ># y) = n
                  | otherwise         = go x0
                 where
                   go x = I# x `c` if isTrue# (x ==# y)
                                   then n
                                   else go (x +# 1#)
                        -- Watch out for y=maxBound; hence ==, not >
        -- Be very careful not to have more than one "c"
        -- so that when eftInfFB is inlined we can inline
        -- whatever is bound to "c"


-----------------------------------------------------
-- efdInt and efdtInt deal with [a,b..] and [a,b..c].
-- The code is more complicated because of worries about Int overflow.

{-# RULES
"efdtInt"       [~1] forall x1 x2 y.
                     efdtInt x1 x2 y = build (\ c n -> efdtIntFB c n x1 x2 y)
"efdtIntUpList" [1]  efdtIntFB (:) [] = efdtInt
 #-}

efdInt :: Int# -> Int# -> [Int]
-- [x1,x2..maxInt]
efdInt x1 x2
 | isTrue# (x2 >=# x1) = case maxInt of I# y -> efdtIntUp x1 x2 y
 | otherwise           = case minInt of I# y -> efdtIntDn x1 x2 y

{-# NOINLINE [1] efdtInt #-}
efdtInt :: Int# -> Int# -> Int# -> [Int]
-- [x1,x2..y]
efdtInt x1 x2 y
 | isTrue# (x2 >=# x1) = efdtIntUp x1 x2 y
 | otherwise           = efdtIntDn x1 x2 y

{-# INLINE [0] efdtIntFB #-}
efdtIntFB :: (Int -> r -> r) -> r -> Int# -> Int# -> Int# -> r
efdtIntFB c n x1 x2 y
 | isTrue# (x2 >=# x1) = efdtIntUpFB c n x1 x2 y
 | otherwise           = efdtIntDnFB c n x1 x2 y

-- Requires x2 >= x1
efdtIntUp :: Int# -> Int# -> Int# -> [Int]
efdtIntUp x1 x2 y    -- Be careful about overflow!
 | isTrue# (y <# x2) = if isTrue# (y <# x1) then [] else [I# x1]
 | otherwise = -- Common case: x1 <= x2 <= y
               let !delta = x2 -# x1 -- >= 0
                   !y' = y -# delta  -- x1 <= y' <= y; hence y' is representable

                   -- Invariant: x <= y
                   -- Note that: z <= y' => z + delta won't overflow
                   -- so we are guaranteed not to overflow if/when we recurse
                   go_up x | isTrue# (x ># y') = [I# x]
                           | otherwise         = I# x : go_up (x +# delta)
               in I# x1 : go_up x2

-- Requires x2 >= x1
efdtIntUpFB :: (Int -> r -> r) -> r -> Int# -> Int# -> Int# -> r
efdtIntUpFB c n x1 x2 y    -- Be careful about overflow!
 | isTrue# (y <# x2) = if isTrue# (y <# x1) then n else I# x1 `c` n
 | otherwise = -- Common case: x1 <= x2 <= y
               let !delta = x2 -# x1 -- >= 0
                   !y' = y -# delta  -- x1 <= y' <= y; hence y' is representable

                   -- Invariant: x <= y
                   -- Note that: z <= y' => z + delta won't overflow
                   -- so we are guaranteed not to overflow if/when we recurse
                   go_up x | isTrue# (x ># y') = I# x `c` n
                           | otherwise         = I# x `c` go_up (x +# delta)
               in I# x1 `c` go_up x2

-- Requires x2 <= x1
efdtIntDn :: Int# -> Int# -> Int# -> [Int]
efdtIntDn x1 x2 y    -- Be careful about underflow!
 | isTrue# (y ># x2) = if isTrue# (y ># x1) then [] else [I# x1]
 | otherwise = -- Common case: x1 >= x2 >= y
               let !delta = x2 -# x1 -- <= 0
                   !y' = y -# delta  -- y <= y' <= x1; hence y' is representable

                   -- Invariant: x >= y
                   -- Note that: z >= y' => z + delta won't underflow
                   -- so we are guaranteed not to underflow if/when we recurse
                   go_dn x | isTrue# (x <# y') = [I# x]
                           | otherwise         = I# x : go_dn (x +# delta)
   in I# x1 : go_dn x2

-- Requires x2 <= x1
efdtIntDnFB :: (Int -> r -> r) -> r -> Int# -> Int# -> Int# -> r
efdtIntDnFB c n x1 x2 y    -- Be careful about underflow!
 | isTrue# (y ># x2) = if isTrue# (y ># x1) then n else I# x1 `c` n
 | otherwise = -- Common case: x1 >= x2 >= y
               let !delta = x2 -# x1 -- <= 0
                   !y' = y -# delta  -- y <= y' <= x1; hence y' is representable

                   -- Invariant: x >= y
                   -- Note that: z >= y' => z + delta won't underflow
                   -- so we are guaranteed not to underflow if/when we recurse
                   go_dn x | isTrue# (x <# y') = I# x `c` n
                           | otherwise         = I# x `c` go_dn (x +# delta)
               in I# x1 `c` go_dn x2
\end{code}


%*********************************************************
%*                                                      *
\subsection{Type @Word@}
%*                                                      *
%*********************************************************

\begin{code}
instance Bounded Word where
    minBound = 0

    -- use unboxed literals for maxBound, because GHC doesn't optimise
    -- (fromInteger 0xffffffff :: Word).
#if WORD_SIZE_IN_BITS == 32
    maxBound = W# (int2Word# 0xFFFFFFFF#)
#elif WORD_SIZE_IN_BITS == 64
    maxBound = W# (int2Word# 0xFFFFFFFFFFFFFFFF#)
#else
#error Unhandled value for WORD_SIZE_IN_BITS
#endif
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
    fromEnum n           = I# (integerToInt n)

    {-# INLINE enumFrom #-}
    {-# INLINE enumFromThen #-}
    {-# INLINE enumFromTo #-}
    {-# INLINE enumFromThenTo #-}
    enumFrom x             = enumDeltaInteger   x 1
    enumFromThen x y       = enumDeltaInteger   x (y-x)
    enumFromTo x lim       = enumDeltaToInteger x 1     lim
    enumFromThenTo x y lim = enumDeltaToInteger x (y-x) lim

{-# RULES
"enumDeltaInteger"      [~1] forall x y.  enumDeltaInteger x y     = build (\c _ -> enumDeltaIntegerFB c x y)
"efdtInteger"           [~1] forall x y l.enumDeltaToInteger x y l = build (\c n -> enumDeltaToIntegerFB c n x y l)
"enumDeltaInteger"      [1] enumDeltaIntegerFB   (:)    = enumDeltaInteger
"enumDeltaToInteger"    [1] enumDeltaToIntegerFB (:) [] = enumDeltaToInteger
 #-}

{-# NOINLINE [0] enumDeltaIntegerFB #-}
enumDeltaIntegerFB :: (Integer -> b -> b) -> Integer -> Integer -> b
enumDeltaIntegerFB c x d = x `seq` (x `c` enumDeltaIntegerFB c (x+d) d)

{-# NOINLINE [1] enumDeltaInteger #-}
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

{-# RULES
"enumDeltaToInteger1"   [0] forall c n x . enumDeltaToIntegerFB c n x 1 = up_fb c n x 1
 #-}
-- This rule ensures that in the common case (delta = 1), we do not do the check here,
-- and also that we have the chance to inline up_fb, which would allow the constuctor to be
-- inlined and good things to happen.
-- We do not do it for Int this way because hand-tuned code already exists, and
-- the special case varies more from the general case, due to the issue of overflows.

{-# NOINLINE [1] enumDeltaToInteger #-}
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

