{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK not-home #-}

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

import GHC.Base hiding ( many )
import GHC.Char
import GHC.Num.Integer
import GHC.Num
import GHC.Show
import GHC.Tuple (Solo (..))
default ()              -- Double isn't available yet

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

    -- | Used in Haskell's translation of @[n..]@ with @[n..] = enumFrom n@,
    --   a possible implementation being @enumFrom n = n : enumFrom (succ n)@.
    --   For example:
    --
    --     * @enumFrom 4 :: [Integer] = [4,5,6,7,...]@
    --     * @enumFrom 6 :: [Int] = [6,7,8,9,...,maxBound :: Int]@
    enumFrom            :: a -> [a]
    -- | Used in Haskell's translation of @[n,n'..]@
    --   with @[n,n'..] = enumFromThen n n'@, a possible implementation being
    --   @enumFromThen n n' = n : n' : worker (f x) (f x n')@,
    --   @worker s v = v : worker s (s v)@, @x = fromEnum n' - fromEnum n@ and
    --   @f n y
    --     | n > 0 = f (n - 1) (succ y)
    --     | n < 0 = f (n + 1) (pred y)
    --     | otherwise = y@
    --   For example:
    --
    --     * @enumFromThen 4 6 :: [Integer] = [4,6,8,10...]@
    --     * @enumFromThen 6 2 :: [Int] = [6,2,-2,-6,...,minBound :: Int]@
    enumFromThen        :: a -> a -> [a]
    -- | Used in Haskell's translation of @[n..m]@ with
    --   @[n..m] = enumFromTo n m@, a possible implementation being
    --   @enumFromTo n m
    --      | n <= m = n : enumFromTo (succ n) m
    --      | otherwise = []@.
    --   For example:
    --
    --     * @enumFromTo 6 10 :: [Int] = [6,7,8,9,10]@
    --     * @enumFromTo 42 1 :: [Integer] = []@
    enumFromTo          :: a -> a -> [a]
    -- | Used in Haskell's translation of @[n,n'..m]@ with
    --   @[n,n'..m] = enumFromThenTo n n' m@, a possible implementation
    --   being @enumFromThenTo n n' m = worker (f x) (c x) n m@,
    --   @x = fromEnum n' - fromEnum n@, @c x = bool (>=) (<=) (x > 0)@
    --   @f n y
    --      | n > 0 = f (n - 1) (succ y)
    --      | n < 0 = f (n + 1) (pred y)
    --      | otherwise = y@ and
    --   @worker s c v m
    --      | c v m = v : worker s c (s v) m
    --      | otherwise = []@
    --   For example:
    --
    --     * @enumFromThenTo 4 2 -6 :: [Integer] = [4,2,0,-2,-4,-6]@
    --     * @enumFromThenTo 6 8 2 :: [Int] = []@
    enumFromThenTo      :: a -> a -> a -> [a]

    succ = toEnum . (+ 1) . fromEnum

    pred = toEnum . (subtract 1) . fromEnum

    -- See Note [Stable Unfolding for list producers]
    {-# INLINABLE enumFrom #-}
    enumFrom x = map toEnum [fromEnum x ..]

    -- See Note [Stable Unfolding for list producers]
    {-# INLINABLE enumFromThen #-}
    enumFromThen x y = map toEnum [fromEnum x, fromEnum y ..]

    -- See Note [Stable Unfolding for list producers]
    {-# INLINABLE enumFromTo #-}
    enumFromTo x y = map toEnum [fromEnum x .. fromEnum y]

    -- See Note [Stable Unfolding for list producers]
    {-# INLINABLE enumFromThenTo #-}
    enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

-- See Note [Stable Unfolding for list producers]
{-# INLINABLE boundedEnumFrom #-}
-- Default methods for bounded enumerations
boundedEnumFrom :: (Enum a, Bounded a) => a -> [a]
boundedEnumFrom n = map toEnum [fromEnum n .. fromEnum (maxBound `asTypeOf` n)]

-- See Note [Stable Unfolding for list producers]
{-# INLINABLE boundedEnumFromThen #-}
boundedEnumFromThen :: (Enum a, Bounded a) => a -> a -> [a]
boundedEnumFromThen n1 n2
  | i_n2 >= i_n1  = map toEnum [i_n1, i_n2 .. fromEnum (maxBound `asTypeOf` n1)]
  | otherwise     = map toEnum [i_n1, i_n2 .. fromEnum (minBound `asTypeOf` n1)]
  where
    i_n1 = fromEnum n1
    i_n2 = fromEnum n2

{-
Note [Stable Unfolding for list producers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The INLINABLE/INLINE pragmas ensure that we export stable (unoptimised)
unfoldings in the interface file so we can do list fusion at usage sites.
-}

------------------------------------------------------------------------
-- Helper functions
------------------------------------------------------------------------

{-# NOINLINE toEnumError #-}
toEnumError :: (Show a) => String -> Int -> (a,a) -> b
toEnumError inst_ty i bnds =
    errorWithoutStackTrace $ "Enum.toEnum{" ++ inst_ty ++ "}: tag (" ++
            show i ++
            ") is outside of bounds " ++
            show bnds

{-# NOINLINE fromEnumError #-}
fromEnumError :: (Show a) => String -> a -> b
fromEnumError inst_ty x =
    errorWithoutStackTrace $ "Enum.fromEnum{" ++ inst_ty ++ "}: value (" ++
            show x ++
            ") is outside of Int's bounds " ++
            show (minBound::Int, maxBound::Int)

{-# NOINLINE succError #-}
succError :: String -> a
succError inst_ty =
    errorWithoutStackTrace $ "Enum.succ{" ++ inst_ty ++ "}: tried to take `succ' of maxBound"

{-# NOINLINE predError #-}
predError :: String -> a
predError inst_ty =
    errorWithoutStackTrace $ "Enum.pred{" ++ inst_ty ++ "}: tried to take `pred' of minBound"

------------------------------------------------------------------------
-- Tuples
------------------------------------------------------------------------

-- | @since 2.01
deriving instance Bounded ()

-- | @since 2.01
instance Enum () where
    succ _      = errorWithoutStackTrace "Prelude.Enum.().succ: bad argument"
    pred _      = errorWithoutStackTrace "Prelude.Enum.().pred: bad argument"

    toEnum x | x == 0    = ()
             | otherwise = errorWithoutStackTrace "Prelude.Enum.().toEnum: bad argument"

    fromEnum () = 0
    enumFrom ()         = [()]
    enumFromThen () ()  = let many = ():many in many
    enumFromTo () ()    = [()]
    enumFromThenTo () () () = let many = ():many in many

instance Enum a => Enum (Solo a) where
    succ (Solo a) = Solo (succ a)
    pred (Solo a) = Solo (pred a)

    toEnum x = Solo (toEnum x)

    fromEnum (Solo x) = fromEnum x
    enumFrom (Solo x) = [Solo a | a <- enumFrom x]
    enumFromThen (Solo x) (Solo y) =
      [Solo a | a <- enumFromThen x y]
    enumFromTo (Solo x) (Solo y) =
      [Solo a | a <- enumFromTo x y]
    enumFromThenTo (Solo x) (Solo y) (Solo z) =
      [Solo a | a <- enumFromThenTo x y z]

deriving instance Bounded a => Bounded (Solo a)
-- Report requires instances up to 15
-- | @since 2.01
deriving instance (Bounded a, Bounded b)
        => Bounded (a,b)
-- | @since 2.01
deriving instance (Bounded a, Bounded b, Bounded c)
        => Bounded (a,b,c)
-- | @since 2.01
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d)
        => Bounded (a,b,c,d)
-- | @since 2.01
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e)
        => Bounded (a,b,c,d,e)
-- | @since 2.01
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f)
        => Bounded (a,b,c,d,e,f)
-- | @since 2.01
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g)
        => Bounded (a,b,c,d,e,f,g)
-- | @since 2.01
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h)
        => Bounded (a,b,c,d,e,f,g,h)
-- | @since 2.01
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i)
        => Bounded (a,b,c,d,e,f,g,h,i)
-- | @since 2.01
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i, Bounded j)
        => Bounded (a,b,c,d,e,f,g,h,i,j)
-- | @since 2.01
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k)
        => Bounded (a,b,c,d,e,f,g,h,i,j,k)
-- | @since 2.01
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k,
          Bounded l)
        => Bounded (a,b,c,d,e,f,g,h,i,j,k,l)
-- | @since 2.01
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k,
          Bounded l, Bounded m)
        => Bounded (a,b,c,d,e,f,g,h,i,j,k,l,m)
-- | @since 2.01
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k,
          Bounded l, Bounded m, Bounded n)
        => Bounded (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
-- | @since 2.01
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e,
          Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k,
          Bounded l, Bounded m, Bounded n, Bounded o)
        => Bounded (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

------------------------------------------------------------------------
-- Bool
------------------------------------------------------------------------

-- | @since 2.01
deriving instance Bounded Bool

-- | @since 2.01
instance Enum Bool where
  succ False = True
  succ True  = errorWithoutStackTrace "Prelude.Enum.Bool.succ: bad argument"

  pred True  = False
  pred False  = errorWithoutStackTrace "Prelude.Enum.Bool.pred: bad argument"

  toEnum n | n == 0    = False
           | n == 1    = True
           | otherwise = errorWithoutStackTrace "Prelude.Enum.Bool.toEnum: bad argument"

  fromEnum False = 0
  fromEnum True  = 1

  -- Use defaults for the rest
  enumFrom     = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

------------------------------------------------------------------------
-- Ordering
------------------------------------------------------------------------

-- | @since 2.01
deriving instance Bounded Ordering
-- | @since 2.01
instance Enum Ordering where
  succ LT = EQ
  succ EQ = GT
  succ GT = errorWithoutStackTrace "Prelude.Enum.Ordering.succ: bad argument"

  pred GT = EQ
  pred EQ = LT
  pred LT = errorWithoutStackTrace "Prelude.Enum.Ordering.pred: bad argument"

  toEnum n | n == 0 = LT
           | n == 1 = EQ
           | n == 2 = GT
  toEnum _ = errorWithoutStackTrace "Prelude.Enum.Ordering.toEnum: bad argument"

  fromEnum LT = 0
  fromEnum EQ = 1
  fromEnum GT = 2

  -- Use defaults for the rest
  enumFrom     = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

------------------------------------------------------------------------
-- Char
------------------------------------------------------------------------

-- | @since 2.01
instance  Bounded Char  where
    minBound =  '\0'
    maxBound =  '\x10FFFF'

-- | @since 2.01
instance  Enum Char  where
    succ (C# c#)
       | isTrue# (ord# c# /=# 0x10FFFF#) = C# (chr# (ord# c# +# 1#))
       | otherwise             = errorWithoutStackTrace ("Prelude.Enum.Char.succ: bad argument")
    pred (C# c#)
       | isTrue# (ord# c# /=# 0#) = C# (chr# (ord# c# -# 1#))
       | otherwise                = errorWithoutStackTrace ("Prelude.Enum.Char.pred: bad argument")

    toEnum   = chr
    fromEnum = ord

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFrom #-}
    enumFrom (C# x) = eftChar (ord# x) 0x10FFFF#
        -- Blarg: technically I guess enumFrom isn't strict!

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFromTo #-}
    enumFromTo (C# x) (C# y) = eftChar (ord# x) (ord# y)

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFromThen #-}
    enumFromThen (C# x1) (C# x2) = efdChar (ord# x1) (ord# x2)

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFromThenTo #-}
    enumFromThenTo (C# x1) (C# x2) (C# y) = efdtChar (ord# x1) (ord# x2) (ord# y)

-- See Note [How the Enum rules work]
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
{-# INLINE [0] eftCharFB #-} -- See Note [Inline FB functions] in GHC.List
eftCharFB :: (Char -> a -> a) -> a -> Int# -> Int# -> a
eftCharFB c n x0 y = go x0
                 where
                    go x | isTrue# (x ># y) = n
                         | otherwise        = C# (chr# x) `c` go (x +# 1#)

{-# NOINLINE [1] eftChar #-} -- Inline after rule "eftChar" is inactive
eftChar :: Int# -> Int# -> String
eftChar x y | isTrue# (x ># y ) = []
            | otherwise         = C# (chr# x) : eftChar (x +# 1#) y


-- For enumFromThenTo we give up on inlining
{-# INLINE [0] efdCharFB #-} -- See Note [Inline FB functions] in GHC.List
efdCharFB :: (Char -> a -> a) -> a -> Int# -> Int# -> a
efdCharFB c n x1 x2
  | isTrue# (delta >=# 0#) = go_up_char_fb c n x1 delta 0x10FFFF#
  | otherwise              = go_dn_char_fb c n x1 delta 0#
  where
    !delta = x2 -# x1

{-# NOINLINE [1] efdChar #-} -- Inline after rule "efdChar" is inactive
efdChar :: Int# -> Int# -> String
efdChar x1 x2
  | isTrue# (delta >=# 0#) = go_up_char_list x1 delta 0x10FFFF#
  | otherwise              = go_dn_char_list x1 delta 0#
  where
    !delta = x2 -# x1

{-# INLINE [0] efdtCharFB #-} -- See Note [Inline FB functions] in GHC.List
efdtCharFB :: (Char -> a -> a) -> a -> Int# -> Int# -> Int# -> a
efdtCharFB c n x1 x2 lim
  | isTrue# (delta >=# 0#) = go_up_char_fb c n x1 delta lim
  | otherwise              = go_dn_char_fb c n x1 delta lim
  where
    !delta = x2 -# x1

{-# NOINLINE [1] efdtChar #-} -- Inline after rule "efdtChar" is inactive
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


------------------------------------------------------------------------
-- Int
------------------------------------------------------------------------

{-
Be careful about these instances.
        (a) remember that you have to count down as well as up e.g. [13,12..0]
        (b) be careful of Int overflow
        (c) remember that Int is bounded, so [1..] terminates at maxInt
-}

-- | @since 2.01
instance  Bounded Int where
    minBound =  minInt
    maxBound =  maxInt

-- | @since 2.01
instance  Enum Int  where
    succ x
       | x == maxBound  = errorWithoutStackTrace "Prelude.Enum.succ{Int}: tried to take `succ' of maxBound"
       | otherwise      = x + 1
    pred x
       | x == minBound  = errorWithoutStackTrace "Prelude.Enum.pred{Int}: tried to take `pred' of minBound"
       | otherwise      = x - 1

    toEnum   x = x
    fromEnum x = x

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFrom #-}
    enumFrom (I# x) = eftInt x maxInt#
        where !(I# maxInt#) = maxInt
        -- Blarg: technically I guess enumFrom isn't strict!

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFromTo #-}
    enumFromTo (I# x) (I# y) = eftInt x y

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFromThen #-}
    enumFromThen (I# x1) (I# x2) = efdInt x1 x2

    -- See Note [Stable Unfolding for list producers]
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

{- Note [How the Enum rules work]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Phase 2: eftInt ---> build . eftIntFB
* Phase 1: inline build; eftIntFB (:) --> eftInt
* Phase 0: optionally inline eftInt
-}

{-# NOINLINE [1] eftInt #-}
eftInt :: Int# -> Int# -> [Int]
-- [x1..x2]
eftInt x0 y | isTrue# (x0 ># y) = []
            | otherwise         = go x0
               where
                 go x = I# x : if isTrue# (x ==# y)
                               then []
                               else go (x +# 1#)

{-# INLINE [0] eftIntFB #-} -- See Note [Inline FB functions] in GHC.List
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

-- See Note [How the Enum rules work]
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

{-# INLINE [0] efdtIntFB #-} -- See Note [Inline FB functions] in GHC.List
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
{-# INLINE [0] efdtIntUpFB #-} -- See Note [Inline FB functions] in GHC.List
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
{-# INLINE [0] efdtIntDnFB #-} -- See Note [Inline FB functions] in GHC.List
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


------------------------------------------------------------------------
-- Word
------------------------------------------------------------------------

-- | @since 2.01
instance Bounded Word where
    minBound = 0

    -- use unboxed literals for maxBound, because GHC doesn't optimise
    -- (fromInteger 0xffffffff :: Word).
#if WORD_SIZE_IN_BITS == 32
    maxBound = W# 0xFFFFFFFF##
#elif WORD_SIZE_IN_BITS == 64
    maxBound = W# 0xFFFFFFFFFFFFFFFF##
#else
#error Unhandled value for WORD_SIZE_IN_BITS
#endif

-- | @since 2.01
instance Enum Word where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Word"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Word"
    toEnum i@(I# i#)
        | i >= 0        = W# (int2Word# i#)
        | otherwise     = toEnumError "Word" i (minBound::Word, maxBound::Word)
    fromEnum x@(W# x#)
        | x <= maxIntWord = I# (word2Int# x#)
        | otherwise       = fromEnumError "Word" x

    {-# INLINE enumFrom #-}
    enumFrom (W# x#)      = eftWord x# maxWord#
        where !(W# maxWord#) = maxBound
        -- Blarg: technically I guess enumFrom isn't strict!

    {-# INLINE enumFromTo #-}
    enumFromTo (W# x) (W# y) = eftWord x y

    {-# INLINE enumFromThen #-}
    enumFromThen (W# x1) (W# x2) = efdWord x1 x2

    {-# INLINE enumFromThenTo #-}
    enumFromThenTo (W# x1) (W# x2) (W# y) = efdtWord x1 x2 y

maxIntWord :: Word
-- The biggest word representable as an Int
maxIntWord = W# (case maxInt of I# i -> int2Word# i)

-----------------------------------------------------
-- eftWord and eftWordFB deal with [a..b], which is the
-- most common form, so we take a lot of care
-- In particular, we have rules for deforestation

{-# RULES
"eftWord"        [~1] forall x y. eftWord x y = build (\ c n -> eftWordFB c n x y)
"eftWordList"    [1] eftWordFB  (:) [] = eftWord
 #-}

-- The Enum rules for Word work much the same way that they do for Int.
-- See Note [How the Enum rules work].

{-# NOINLINE [1] eftWord #-}
eftWord :: Word# -> Word# -> [Word]
-- [x1..x2]
eftWord x0 y | isTrue# (x0 `gtWord#` y) = []
             | otherwise                = go x0
                where
                  go x = W# x : if isTrue# (x `eqWord#` y)
                                then []
                                else go (x `plusWord#` 1##)

{-# INLINE [0] eftWordFB #-} -- See Note [Inline FB functions] in GHC.List
eftWordFB :: (Word -> r -> r) -> r -> Word# -> Word# -> r
eftWordFB c n x0 y | isTrue# (x0 `gtWord#` y) = n
                   | otherwise                = go x0
                  where
                    go x = W# x `c` if isTrue# (x `eqWord#` y)
                                    then n
                                    else go (x `plusWord#` 1##)
                        -- Watch out for y=maxBound; hence ==, not >
        -- Be very careful not to have more than one "c"
        -- so that when eftInfFB is inlined we can inline
        -- whatever is bound to "c"


-----------------------------------------------------
-- efdWord and efdtWord deal with [a,b..] and [a,b..c].
-- The code is more complicated because of worries about Word overflow.

-- See Note [How the Enum rules work]
{-# RULES
"efdtWord"       [~1] forall x1 x2 y.
                     efdtWord x1 x2 y = build (\ c n -> efdtWordFB c n x1 x2 y)
"efdtWordUpList" [1]  efdtWordFB (:) [] = efdtWord
 #-}

efdWord :: Word# -> Word# -> [Word]
-- [x1,x2..maxWord]
efdWord x1 x2
 | isTrue# (x2 `geWord#` x1) = case maxBound of W# y -> efdtWordUp x1 x2 y
 | otherwise                 = case minBound of W# y -> efdtWordDn x1 x2 y

{-# NOINLINE [1] efdtWord #-}
efdtWord :: Word# -> Word# -> Word# -> [Word]
-- [x1,x2..y]
efdtWord x1 x2 y
 | isTrue# (x2 `geWord#` x1) = efdtWordUp x1 x2 y
 | otherwise                 = efdtWordDn x1 x2 y

{-# INLINE [0] efdtWordFB #-} -- See Note [Inline FB functions] in GHC.List
efdtWordFB :: (Word -> r -> r) -> r -> Word# -> Word# -> Word# -> r
efdtWordFB c n x1 x2 y
 | isTrue# (x2 `geWord#` x1) = efdtWordUpFB c n x1 x2 y
 | otherwise                 = efdtWordDnFB c n x1 x2 y

-- Requires x2 >= x1
efdtWordUp :: Word# -> Word# -> Word# -> [Word]
efdtWordUp x1 x2 y    -- Be careful about overflow!
 | isTrue# (y `ltWord#` x2) = if isTrue# (y `ltWord#` x1) then [] else [W# x1]
 | otherwise = -- Common case: x1 <= x2 <= y
               let !delta = x2 `minusWord#` x1 -- >= 0
                   !y' = y `minusWord#` delta  -- x1 <= y' <= y; hence y' is representable

                   -- Invariant: x <= y
                   -- Note that: z <= y' => z + delta won't overflow
                   -- so we are guaranteed not to overflow if/when we recurse
                   go_up x | isTrue# (x `gtWord#` y') = [W# x]
                           | otherwise                = W# x : go_up (x `plusWord#` delta)
               in W# x1 : go_up x2

-- Requires x2 >= x1
{-# INLINE [0] efdtWordUpFB #-} -- See Note [Inline FB functions] in GHC.List
efdtWordUpFB :: (Word -> r -> r) -> r -> Word# -> Word# -> Word# -> r
efdtWordUpFB c n x1 x2 y    -- Be careful about overflow!
 | isTrue# (y `ltWord#` x2) = if isTrue# (y `ltWord#` x1) then n else W# x1 `c` n
 | otherwise = -- Common case: x1 <= x2 <= y
               let !delta = x2 `minusWord#` x1 -- >= 0
                   !y' = y `minusWord#` delta  -- x1 <= y' <= y; hence y' is representable

                   -- Invariant: x <= y
                   -- Note that: z <= y' => z + delta won't overflow
                   -- so we are guaranteed not to overflow if/when we recurse
                   go_up x | isTrue# (x `gtWord#` y') = W# x `c` n
                           | otherwise                = W# x `c` go_up (x `plusWord#` delta)
               in W# x1 `c` go_up x2

-- Requires x2 <= x1
efdtWordDn :: Word# -> Word# -> Word# -> [Word]
efdtWordDn x1 x2 y    -- Be careful about underflow!
 | isTrue# (y `gtWord#` x2) = if isTrue# (y `gtWord#` x1) then [] else [W# x1]
 | otherwise = -- Common case: x1 >= x2 >= y
               let !delta = x2 `minusWord#` x1 -- <= 0
                   !y' = y `minusWord#` delta  -- y <= y' <= x1; hence y' is representable

                   -- Invariant: x >= y
                   -- Note that: z >= y' => z + delta won't underflow
                   -- so we are guaranteed not to underflow if/when we recurse
                   go_dn x | isTrue# (x `ltWord#` y') = [W# x]
                           | otherwise                = W# x : go_dn (x `plusWord#` delta)
   in W# x1 : go_dn x2

-- Requires x2 <= x1
{-# INLINE [0] efdtWordDnFB #-} -- See Note [Inline FB functions] in GHC.List
efdtWordDnFB :: (Word -> r -> r) -> r -> Word# -> Word# -> Word# -> r
efdtWordDnFB c n x1 x2 y    -- Be careful about underflow!
 | isTrue# (y `gtWord#` x2) = if isTrue# (y `gtWord#` x1) then n else W# x1 `c` n
 | otherwise = -- Common case: x1 >= x2 >= y
               let !delta = x2 `minusWord#` x1 -- <= 0
                   !y' = y `minusWord#` delta  -- y <= y' <= x1; hence y' is representable

                   -- Invariant: x >= y
                   -- Note that: z >= y' => z + delta won't underflow
                   -- so we are guaranteed not to underflow if/when we recurse
                   go_dn x | isTrue# (x `ltWord#` y') = W# x `c` n
                           | otherwise                = W# x `c` go_dn (x `plusWord#` delta)
               in W# x1 `c` go_dn x2

------------------------------------------------------------------------
-- Integer
------------------------------------------------------------------------

-- | @since 2.01
instance  Enum Integer  where
    succ x               = x + 1
    pred x               = x - 1
    toEnum (I# n)        = IS n
    fromEnum n           = integerToInt n

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFrom #-}
    enumFrom x = enumDeltaInteger x 1

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFromThen #-}
    enumFromThen x y = enumDeltaInteger x (y-x)

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFromTo #-}
    enumFromTo x lim = enumDeltaToInteger x 1 lim

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFromThenTo #-}
    enumFromThenTo x y lim = enumDeltaToInteger x (y-x) lim

-- See Note [How the Enum rules work]
{-# RULES
"enumDeltaInteger"      [~1] forall x y.   enumDeltaInteger x y         = build (\c _ -> enumDeltaIntegerFB c x y)
"efdtInteger"           [~1] forall x d l. enumDeltaToInteger x d l     = build (\c n -> enumDeltaToIntegerFB  c n x d l)
"efdtInteger1"          [~1] forall x l.   enumDeltaToInteger x 1 l     = build (\c n -> enumDeltaToInteger1FB c n x l)

"enumDeltaToInteger1FB" [1] forall c n x.  enumDeltaToIntegerFB c n x 1 = enumDeltaToInteger1FB c n x

"enumDeltaInteger"      [1] enumDeltaIntegerFB    (:)     = enumDeltaInteger
"enumDeltaToInteger"    [1] enumDeltaToIntegerFB  (:) []  = enumDeltaToInteger
"enumDeltaToInteger1"   [1] enumDeltaToInteger1FB (:) []  = enumDeltaToInteger1
 #-}

{- Note [Enum Integer rules for literal 1]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The "1" rules above specialise for the common case where delta = 1,
so that we can avoid the delta>=0 test in enumDeltaToIntegerFB.
Then enumDeltaToInteger1FB is nice and small and can be inlined,
which would allow the constructor to be inlined and good things to happen.

We match on the literal "1" both in phase 2 (rule "efdtInteger1") and
phase 1 (rule "enumDeltaToInteger1FB"), just for belt and braces

We do not do it for Int this way because hand-tuned code already exists, and
the special case varies more from the general case, due to the issue of overflows.
-}

{-# INLINE [0] enumDeltaIntegerFB #-}
-- See Note [Inline FB functions] in GHC.List
enumDeltaIntegerFB :: (Integer -> b -> b) -> Integer -> Integer -> b
enumDeltaIntegerFB c x0 d = go x0
  where go x = x `seq` (x `c` go (x+d))

{-# NOINLINE [1] enumDeltaInteger #-} -- Inline after rule "enumDeltaInteger" is inactive
enumDeltaInteger :: Integer -> Integer -> [Integer]
enumDeltaInteger x d = x `seq` (x : enumDeltaInteger (x+d) d)
-- strict accumulator, so
--     head (drop 1000000 [1 .. ]
-- works

{-# INLINE [0] enumDeltaToIntegerFB #-}
-- See Note [Inline FB functions] in GHC.List
-- Don't inline this until RULE "enumDeltaToInteger" has had a chance to fire
enumDeltaToIntegerFB :: (Integer -> a -> a) -> a
                     -> Integer -> Integer -> Integer -> a
enumDeltaToIntegerFB c n x delta lim
  | delta >= 0 = up_fb c n x delta lim
  | otherwise  = dn_fb c n x delta lim

{-# INLINE [0] enumDeltaToInteger1FB #-}
-- See Note [Inline FB functions] in GHC.List
-- Don't inline this until RULE "enumDeltaToInteger" has had a chance to fire
enumDeltaToInteger1FB :: (Integer -> a -> a) -> a
                      -> Integer -> Integer -> a
enumDeltaToInteger1FB c n x0 lim = go (x0 :: Integer)
                      where
                        go x | x > lim   = n
                             | otherwise = x `c` go (x+1)

{-# NOINLINE [1] enumDeltaToInteger #-} -- Inline after rule "efdtInteger" is inactive
enumDeltaToInteger :: Integer -> Integer -> Integer -> [Integer]
enumDeltaToInteger x delta lim
  | delta >= 0 = up_list x delta lim
  | otherwise  = dn_list x delta lim

{-# NOINLINE [1] enumDeltaToInteger1 #-} -- Inline after rule "efdtInteger1" is inactive
enumDeltaToInteger1 :: Integer -> Integer -> [Integer]
-- Special case for Delta = 1
enumDeltaToInteger1 x0 lim = go (x0 :: Integer)
                      where
                        go x | x > lim   = []
                             | otherwise = x : go (x+1)

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

------------------------------------------------------------------------
-- Natural
------------------------------------------------------------------------

-- | @since 4.8.0.0
instance Enum Natural where
    succ n = n + 1
    pred n = n - 1
    toEnum i@(I# i#)
      | i >= 0    = naturalFromWord# (int2Word# i#)
      | otherwise = errorWithoutStackTrace "toEnum: unexpected negative Int"

    fromEnum (NS w) | i >= 0 = i
      where
        i = I# (word2Int# w)
    fromEnum _ = errorWithoutStackTrace "fromEnum: out of Int range"

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFrom #-}
    enumFrom x        = enumDeltaNatural      x 1

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFromThen #-}
    enumFromThen x y
      | x <= y        = enumDeltaNatural      x (y-x)
      | otherwise     = enumNegDeltaToNatural x (x-y) 0

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFromTo #-}
    enumFromTo x lim  = enumDeltaToNatural    x 1 lim

    -- See Note [Stable Unfolding for list producers]
    {-# INLINE enumFromThenTo #-}
    enumFromThenTo x y lim
      | x <= y        = enumDeltaToNatural    x (y-x) lim
      | otherwise     = enumNegDeltaToNatural x (x-y) lim

-- Helpers for 'Enum Natural'; TODO: optimise & make fusion work

enumDeltaNatural :: Natural -> Natural -> [Natural]
enumDeltaNatural !x d = x : enumDeltaNatural (x+d) d

-- Inline to specialize
{-# INLINE enumDeltaToNatural #-}
enumDeltaToNatural :: Natural -> Natural -> Natural -> [Natural]
enumDeltaToNatural x0 delta lim = go x0
  where
    go x | x > lim   = []
         | otherwise = x : go (x+delta)

-- Inline to specialize
{-# INLINE enumNegDeltaToNatural #-}
enumNegDeltaToNatural :: Natural -> Natural -> Natural -> [Natural]
enumNegDeltaToNatural x0 ndelta lim = go x0
  where
    go x | x < lim     = []
         | x >= ndelta = x : go (x-ndelta)
         | otherwise   = [x]


-- Instances from GHC.Types

-- | @since 4.16.0.0
deriving instance Bounded Levity
-- | @since 4.16.0.0
deriving instance Enum Levity

-- | @since 4.10.0.0
deriving instance Bounded VecCount
-- | @since 4.10.0.0
deriving instance Enum VecCount

-- | @since 4.10.0.0
deriving instance Bounded VecElem
-- | @since 4.10.0.0
deriving instance Enum VecElem
