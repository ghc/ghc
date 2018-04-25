{-# LANGUAGE NoImplicitPrelude #-}
module Algebra.Additive (
    -- * Class
    C,
    zero,
    (+), (-),
    negate, subtract,

    -- * Complex functions
    sum, sum1,
    sumNestedAssociative,
    sumNestedCommutative,

    -- * Instance definition helpers
    elementAdd, elementSub, elementNeg,
    (<*>.+), (<*>.-), (<*>.-$),

    -- * Instances for atomic types
    propAssociative,
    propCommutative,
    propIdentity,
    propInverse,
  ) where

import qualified Algebra.Laws as Laws

import Data.Int  (Int,  Int8,  Int16,  Int32,  Int64,  )
import Data.Word (Word, Word8, Word16, Word32, Word64, )

import qualified NumericPrelude.Elementwise as Elem
import Control.Applicative (Applicative(pure, (<*>)), )
import Data.Tuple.HT (fst3, snd3, thd3, )
import qualified Data.List.Match as Match

import qualified Data.Complex as Complex98
import qualified Data.Ratio as Ratio98
import qualified Prelude as P
import Prelude (Integer, Float, Double, fromInteger, )
import NumericPrelude.Base


infixl 6  +, -

{- |
Additive a encapsulates the notion of a commutative group, specified
by the following laws:

@
          a + b === b + a
    (a + b) + c === a + (b + c)
       zero + a === a
   a + negate a === 0
@

Typical examples include integers, dollars, and vectors.

Minimal definition: '+', 'zero', and ('negate' or '(-)')
-}

class C a where
    {-# MINIMAL zero, (+), ((-) | negate) #-}
    -- | zero element of the vector space
    zero     :: a
    -- | add and subtract elements
    (+), (-) :: a -> a -> a
    -- | inverse with respect to '+'
    negate   :: a -> a

    {-# INLINE negate #-}
    negate a = zero - a
    {-# INLINE (-) #-}
    a - b    = a + negate b

{- |
'subtract' is @(-)@ with swapped operand order.
This is the operand order which will be needed in most cases
of partial application.
-}
subtract :: C a => a -> a -> a
subtract = flip (-)




{- |
Sum up all elements of a list.
An empty list yields zero.

This function is inappropriate for number types like Peano.
Maybe we should make 'sum' a method of Additive.
This would also make 'lengthLeft' and 'lengthRight' superfluous.
-}
sum :: (C a) => [a] -> a
sum = foldl (+) zero

{- |
Sum up all elements of a non-empty list.
This avoids including a zero which is useful for types
where no universal zero is available.
-}
sum1 :: (C a) => [a] -> a
sum1 = foldl1 (+)


{- |
Sum the operands in an order,
such that the dependencies are minimized.
Does this have a measurably effect on speed?

Requires associativity.
-}
sumNestedAssociative :: (C a) => [a] -> a
sumNestedAssociative [] = zero
sumNestedAssociative [x] = x
sumNestedAssociative xs = sumNestedAssociative (sum2 xs)

{-
Make sure that the last entries in the list
are equally often part of an addition.
Maybe this can reduce rounding errors.
The list that sum2 computes is a breadth-first-flattened binary tree.

Requires associativity and commutativity.
-}
sumNestedCommutative :: (C a) => [a] -> a
sumNestedCommutative [] = zero
sumNestedCommutative xs@(_:rs) =
   let ys = xs ++ Match.take rs (sum2 ys)
   in  last ys

_sumNestedCommutative :: (C a) => [a] -> a
_sumNestedCommutative [] = zero
_sumNestedCommutative xs@(_:rs) =
   let ys = xs ++ take (length rs) (sum2 ys)
   in  last ys

{-
[a,b,c, a+b,c+(a+b)]
[a,b,c,d, a+b,c+d,(a+b)+(c+d)]
[a,b,c,d,e, a+b,c+d,e+(a+b),(c+d)+e+(a+b)]
[a,b,c,d,e,f, a+b,c+d,e+f,(a+b)+(c+d),(e+f)+((a+b)+(c+d))]
-}

sum2 :: (C a) => [a] -> [a]
sum2 (x:y:rest) = (x+y) : sum2 rest
sum2 xs = xs



{- |
Instead of baking the add operation into the element function,
we could use higher rank types
and pass a generic @uncurry (+)@ to the run function.
We do not do so in order to stay Haskell 98
at least for parts of NumericPrelude.
-}
{-# INLINE elementAdd #-}
elementAdd ::
   (C x) =>
   (v -> x) -> Elem.T (v,v) x
elementAdd f =
   Elem.element (\(x,y) -> f x + f y)

{-# INLINE elementSub #-}
elementSub ::
   (C x) =>
   (v -> x) -> Elem.T (v,v) x
elementSub f =
   Elem.element (\(x,y) -> f x - f y)

{-# INLINE elementNeg #-}
elementNeg ::
   (C x) =>
   (v -> x) -> Elem.T v x
elementNeg f =
   Elem.element (negate . f)


-- like <*>
infixl 4 <*>.+, <*>.-, <*>.-$

{- |
> addPair :: (Additive.C a, Additive.C b) => (a,b) -> (a,b) -> (a,b)
> addPair = Elem.run2 $ Elem.with (,) <*>.+  fst <*>.+  snd
-}
{-# INLINE (<*>.+) #-}
(<*>.+) ::
   (C x) =>
   Elem.T (v,v) (x -> a) -> (v -> x) -> Elem.T (v,v) a
(<*>.+) f acc =
   f <*> elementAdd acc

{-# INLINE (<*>.-) #-}
(<*>.-) ::
   (C x) =>
   Elem.T (v,v) (x -> a) -> (v -> x) -> Elem.T (v,v) a
(<*>.-) f acc =
   f <*> elementSub acc

{-# INLINE (<*>.-$) #-}
(<*>.-$) ::
   (C x) =>
   Elem.T v (x -> a) -> (v -> x) -> Elem.T v a
(<*>.-$) f acc =
   f <*> elementNeg acc


-- * Instances for atomic types

instance C Integer where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = P.fromInteger 0
   negate = P.negate
   (+)    = (P.+)
   (-)    = (P.-)

instance C Float   where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = P.fromInteger 0
   negate = P.negate
   (+)    = (P.+)
   (-)    = (P.-)

instance C Double  where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = P.fromInteger 0
   negate = P.negate
   (+)    = (P.+)
   (-)    = (P.-)


instance C Int     where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = P.fromInteger 0
   negate = P.negate
   (+)    = (P.+)
   (-)    = (P.-)

instance C Int8    where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = P.fromInteger 0
   negate = P.negate
   (+)    = (P.+)
   (-)    = (P.-)

instance C Int16   where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = P.fromInteger 0
   negate = P.negate
   (+)    = (P.+)
   (-)    = (P.-)

instance C Int32   where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = P.fromInteger 0
   negate = P.negate
   (+)    = (P.+)
   (-)    = (P.-)

instance C Int64   where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = P.fromInteger 0
   negate = P.negate
   (+)    = (P.+)
   (-)    = (P.-)


instance C Word    where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = P.fromInteger 0
   negate = P.negate
   (+)    = (P.+)
   (-)    = (P.-)

instance C Word8   where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = P.fromInteger 0
   negate = P.negate
   (+)    = (P.+)
   (-)    = (P.-)

instance C Word16  where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = P.fromInteger 0
   negate = P.negate
   (+)    = (P.+)
   (-)    = (P.-)

instance C Word32  where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = P.fromInteger 0
   negate = P.negate
   (+)    = (P.+)
   (-)    = (P.-)

instance C Word64  where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = P.fromInteger 0
   negate = P.negate
   (+)    = (P.+)
   (-)    = (P.-)




-- * Instances for composed types

instance (C v0, C v1) => C (v0, v1) where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = (,) zero zero
   (+)    = Elem.run2 $ pure (,) <*>.+  fst <*>.+  snd
   (-)    = Elem.run2 $ pure (,) <*>.-  fst <*>.-  snd
   negate = Elem.run  $ pure (,) <*>.-$ fst <*>.-$ snd

instance (C v0, C v1, C v2) => C (v0, v1, v2) where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero   = (,,) zero zero zero
   (+)    = Elem.run2 $ pure (,,) <*>.+  fst3 <*>.+  snd3 <*>.+  thd3
   (-)    = Elem.run2 $ pure (,,) <*>.-  fst3 <*>.-  snd3 <*>.-  thd3
   negate = Elem.run  $ pure (,,) <*>.-$ fst3 <*>.-$ snd3 <*>.-$ thd3


instance (C v) => C [v] where
   zero   = []
   negate = map negate
   (+) (x:xs) (y:ys) = (+) x y : (+) xs ys
   (+) xs     []     = xs
   (+) []     ys     = ys
   (-) (x:xs) (y:ys) = (-) x y : (-) xs ys
   (-) xs     []     = xs
   (-) []     ys     = negate ys


instance (C v) => C (b -> v) where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero       _ = zero
   (+)    f g x = (+) (f x) (g x)
   (-)    f g x = (-) (f x) (g x)
   negate f   x = negate (f x)

-- * Properties

propAssociative :: (Eq a, C a) => a -> a -> a -> Bool
propCommutative :: (Eq a, C a) => a -> a -> Bool
propIdentity    :: (Eq a, C a) => a -> Bool
propInverse     :: (Eq a, C a) => a -> Bool

propCommutative  =  Laws.commutative (+)
propAssociative  =  Laws.associative (+)
propIdentity     =  Laws.identity (+) zero
propInverse      =  Laws.inverse (+) negate zero



-- legacy

instance (P.Integral a) => C (Ratio98.Ratio a) where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero                =  P.fromInteger 0
   (+)                 =  (P.+)
   (-)                 =  (P.-)
   negate              =  P.negate

instance (P.RealFloat a) => C (Complex98.Complex a) where
   {-# INLINE zero #-}
   {-# INLINE negate #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   zero                =  P.fromInteger 0
   (+)                 =  (P.+)
   (-)                 =  (P.-)
   negate              =  P.negate
