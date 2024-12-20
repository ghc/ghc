{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, PatternSynonyms, ExplicitNamespaces #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Tuple
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/ghc-internal/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC extensions)
--
-- The tuple data types
--
-- Users should not import this module.  It is GHC internal only.
--
-----------------------------------------------------------------------------

module GHC.Internal.Tuple (
  Tuple0, Tuple1,
  Unit(..),
  Solo (Solo, MkSolo), getSolo,
  Tuple2(..),  Tuple3(..),  Tuple4(..),  Tuple5(..),  Tuple6(..),  Tuple7(..),  Tuple8(..),  Tuple9(..),
  Tuple10(..), Tuple11(..), Tuple12(..), Tuple13(..), Tuple14(..), Tuple15(..), Tuple16(..), Tuple17(..), Tuple18(..), Tuple19(..),
  Tuple20(..), Tuple21(..), Tuple22(..), Tuple23(..), Tuple24(..), Tuple25(..), Tuple26(..), Tuple27(..), Tuple28(..), Tuple29(..),
  Tuple30(..), Tuple31(..), Tuple32(..), Tuple33(..), Tuple34(..), Tuple35(..), Tuple36(..), Tuple37(..), Tuple38(..), Tuple39(..),
  Tuple40(..), Tuple41(..), Tuple42(..), Tuple43(..), Tuple44(..), Tuple45(..), Tuple46(..), Tuple47(..), Tuple48(..), Tuple49(..),
  Tuple50(..), Tuple51(..), Tuple52(..), Tuple53(..), Tuple54(..), Tuple55(..), Tuple56(..), Tuple57(..), Tuple58(..), Tuple59(..),
  Tuple60(..), Tuple61(..), Tuple62(..), Tuple63(..), Tuple64(..),
) where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Internal.Types ()

default () -- Double and Integer aren't available yet

-- | The unit datatype @Unit@ has one non-undefined member, the nullary
-- constructor @()@.
--
-- @since 0.11.0
--
data Unit = ()

-- The desugarer uses 1-tuples,
-- but "Unit" is already used up for 0-tuples
-- See Note [One-tuples] in GHC.Builtin.Types

-- | @Solo@ is the canonical lifted 1-tuple, just like 'Tuple2' is the canonical
-- lifted 2-tuple (pair) and 'Tuple3' is the canonical lifted 3-tuple (triple).
--
-- The most important feature of @Solo@ is that it is possible to force its
-- "outside" (usually by pattern matching) without forcing its "inside",
-- because it is defined as a datatype rather than a newtype. One situation
-- where this can be useful is when writing a function to extract a value from
-- a data structure. Suppose you write an implementation of arrays and offer
-- only this function to index into them:
--
-- @
-- index :: Array a -> Int -> a
-- @
--
-- Now imagine that someone wants to extract a value from an array and store it
-- in a lazy-valued finite map/dictionary:
--
-- @
-- insert "hello" (arr `index` 12) m
-- @
--
-- This can actually lead to a space leak. The value is not actually extracted
-- from the array until that value (now buried in a map) is forced. That means
-- the entire array may be kept live by just that value!  Often, the solution
-- is to use a strict map, or to force the value before storing it, but for
-- some purposes that's undesirable.
--
-- One common solution is to include an indexing function that can produce its
-- result in an arbitrary @Applicative@ context:
--
-- @
-- indexA :: Applicative f => Array a -> Int -> f a
-- @
--
-- When using @indexA@ in a /pure/ context, @Solo@ serves as a handy
-- @Applicative@ functor to hold the result. You could write a non-leaky
-- version of the above example thus:
--
-- @
-- case arr `indexA` 12 of
--   Solo a -> insert "hello" a m
-- @
--
-- While such simple extraction functions are the most common uses for
-- unary tuples, they can also be useful for fine-grained control of
-- strict-spined data structure traversals, and for unifying the
-- implementations of lazy and strict mapping functions.
data Solo a = MkSolo a

-- This will have to wait for https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0065-type-infix.rst
-- Otherwise the deprecation would apply to the data type as well.
{-# DEPRECATED data Solo "The Solo constructor has been renamed to MkSolo to avoid punning." #-}
pattern Solo :: a -> Solo a
pattern Solo x = MkSolo x
{-# COMPLETE Solo #-}

-- | Extract the value from a 'Solo'. Very often, values should be extracted
-- directly using pattern matching, to control just what gets evaluated when.
-- @getSolo@ is for convenience in situations where that is not the case:
--
-- When the result is passed to a /strict/ function, it makes no difference
-- whether the pattern matching is done on the \"outside\" or on the
-- \"inside\":
--
-- @
-- Data.Set.insert (getSolo sol) set === case sol of Solo v -> Data.Set.insert v set
-- @
--
-- A traversal may be performed in 'Solo' in order to control evaluation
-- internally, while using @getSolo@ to extract the final result. A strict
-- mapping function, for example, could be defined
--
-- @
-- map' :: Traversable t => (a -> b) -> t a -> t b
-- map' f = getSolo . traverse ((Solo $!) . f)
-- @
getSolo :: Solo a -> a
-- getSolo is a standalone function, rather than a record field of Solo,
-- because Solo is a wired-in TyCon, and a wired-in TyCon that  has
-- record fields is a bit more inconvenient than if it doesn't.
-- (No other wired-in TyCon has record fields.)  So it seems easier
-- to have getSolo as its own separate function (#20562)
getSolo (MkSolo a) = a

-- | A tuple of zero elements, a synonym for 'Unit'.
--
-- @since 0.11.0
--
type Tuple0 = Unit

-- | A tuple of one element, a synonym for 'Solo'.
--
-- @since 0.11.0
--
type Tuple1 = Solo

-- | A tuple of two elements.
--
-- @since 0.11.0
--
data Tuple2 a b = (a,b)

-- | A tuple of three elements.
--
-- @since 0.11.0
--
data Tuple3 a b c = (a,b,c)

-- | A tuple of four elements.
--
-- @since 0.11.0
--
data Tuple4 a b c d = (a,b,c,d)

-- | A tuple of five elements.
--
-- @since 0.11.0
--
data Tuple5 a b c d e = (a,b,c,d,e)

-- | A tuple of six elements.
--
-- @since 0.11.0
--
data Tuple6 a b c d e f = (a,b,c,d,e,f)

-- | A tuple of seven elements.
--
-- @since 0.11.0
--
data Tuple7 a b c d e f g = (a,b,c,d,e,f,g)

-- | A tuple of eight elements.
--
-- @since 0.11.0
--
data Tuple8 a b c d e f g h = (a,b,c,d,e,f,g,h)

-- | A tuple of nine elements.
--
-- @since 0.11.0
--
data Tuple9 a b c d e f g h i = (a,b,c,d,e,f,g,h,i)

-- | A tuple of ten elements.
--
-- @since 0.11.0
--
data Tuple10 a b c d e f g h i j = (a,b,c,d,e,f,g,h,i,j)

-- | A tuple of eleven elements.
--
-- @since 0.11.0
--
data Tuple11 a b c d e f g h i j k = (a,b,c,d,e,f,g,h,i,j,k)

-- | A tuple of twelve elements.
--
-- @since 0.11.0
--
data Tuple12 a b c d e f g h i j k l = (a,b,c,d,e,f,g,h,i,j,k,l)

-- | A tuple of 13 elements.
--
-- @since 0.11.0
--
data Tuple13 a b c d e f g h i j k l m = (a,b,c,d,e,f,g,h,i,j,k,l,m)

-- | A tuple of 14 elements.
--
-- @since 0.11.0
--
data Tuple14 a b c d e f g h i j k l m n = (a,b,c,d,e,f,g,h,i,j,k,l,m,n)

-- | A tuple of 15 elements.
--
-- @since 0.11.0
--
data Tuple15 a b c d e f g h i j k l m n o = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

-- | A tuple of 16 elements.
--
-- @since 0.11.0
--
data Tuple16 a b c d e f g h i j k l m n o p = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

-- | A tuple of 17 elements.
--
-- @since 0.11.0
--
data Tuple17 a b c d e f g h i j k l m n o p q = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)

-- | A tuple of 18 elements.
--
-- @since 0.11.0
--
data Tuple18 a b c d e f g h i j k l m n o p q r = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)

-- | A tuple of 19 elements.
--
-- @since 0.11.0
--
data Tuple19 a b c d e f g h i j k l m n o p q r s = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)

-- | A tuple of 20 elements.
--
-- @since 0.11.0
--
data Tuple20 a b c d e f g h i j k l m n o p q r s t = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)

-- | A tuple of 21 elements.

--
-- @since 0.11.0
--
data Tuple21 a b c d e f g h i j k l m n o p q r s t u = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)

-- | A tuple of 22 elements.
--
-- @since 0.11.0
--
data Tuple22 a b c d e f g h i j k l m n o p q r s t u v = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)

-- | A tuple of 23 elements.
--
-- @since 0.11.0
--
data Tuple23 a b c d e f g h i j k l m n o p q r s t u v w = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w)

-- | A tuple of 24 elements.
--
-- @since 0.11.0
--
data Tuple24 a b c d e f g h i j k l m n o p q r s t u v w x = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)

-- | A tuple of 25 elements.
--
-- @since 0.11.0
--
data Tuple25 a b c d e f g h i j k l m n o p q r s t u v w x y = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y)

-- | A tuple of 26 elements.
--
-- @since 0.11.0
--
data Tuple26 a b c d e f g h i j k l m n o p q r s t u v w x y z = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)

-- | A tuple of 27 elements.
--
-- @since 0.11.0
--
data Tuple27 a b c d e f g h i j k l m n o p q r s t u v w x y z a1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1)

-- | A tuple of 28 elements.
--
-- @since 0.11.0
--
data Tuple28 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1)

-- | A tuple of 29 elements.
--
-- @since 0.11.0
--
data Tuple29 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1)

-- | A tuple of 30 elements.
--
-- @since 0.11.0
--
data Tuple30 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1)

-- | A tuple of 31 elements.
--
-- @since 0.11.0
--
data Tuple31 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1)

-- | A tuple of 32 elements.
--
-- @since 0.11.0
--
data Tuple32 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1)

-- | A tuple of 33 elements.
--
-- @since 0.11.0
--
data Tuple33 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1)

-- | A tuple of 34 elements.
--
-- @since 0.11.0
--
data Tuple34 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1)

-- | A tuple of 35 elements.
--
-- @since 0.11.0
--
data Tuple35 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1)

-- | A tuple of 36 elements.
--
-- @since 0.11.0
--
data Tuple36 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1)

-- | A tuple of 37 elements.
--
-- @since 0.11.0
--
data Tuple37 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1)

-- | A tuple of 38 elements.
--
-- @since 0.11.0
--
data Tuple38 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1)

-- | A tuple of 39 elements.
--
-- @since 0.11.0
--
data Tuple39 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1)

-- | A tuple of 40 elements.
--
-- @since 0.11.0
--
data Tuple40 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1)

-- | A tuple of 41 elements.
--
-- @since 0.11.0
--
data Tuple41 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1)

-- | A tuple of 42 elements.
--
-- @since 0.11.0
--
data Tuple42 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1)

-- | A tuple of 43 elements.
--
-- @since 0.11.0
--
data Tuple43 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1)

-- | A tuple of 44 elements.
--
-- @since 0.11.0
--
data Tuple44 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1)

-- | A tuple of 45 elements.
--
-- @since 0.11.0
--
data Tuple45 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1)

-- | A tuple of 46 elements.
--
-- @since 0.11.0
--
data Tuple46 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1)

-- | A tuple of 47 elements.
--
-- @since 0.11.0
--
data Tuple47 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1)

-- | A tuple of 48 elements.
--
-- @since 0.11.0
--
data Tuple48 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1)

-- | A tuple of 49 elements.
--
-- @since 0.11.0
--
data Tuple49 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1)

-- | A tuple of 50 elements.
--
-- @since 0.11.0
--
data Tuple50 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1)

-- | A tuple of 51 elements.
--
-- @since 0.11.0
--
data Tuple51 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1)

-- | A tuple of 52 elements.
--
-- @since 0.11.0
--
data Tuple52 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1)

-- | A tuple of 53 elements.
--
-- @since 0.11.0
--
data Tuple53 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2)

-- | A tuple of 54 elements.
--
-- @since 0.11.0
--
data Tuple54 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2)

-- | A tuple of 55 elements.
--
-- @since 0.11.0
--
data Tuple55 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2)

-- | A tuple of 56 elements.
--
-- @since 0.11.0
--
data Tuple56 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2)

-- | A tuple of 57 elements.
--
-- @since 0.11.0
--
data Tuple57 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2)

-- | A tuple of 58 elements.
--
-- @since 0.11.0
--
data Tuple58 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2)

-- | A tuple of 59 elements.
--
-- @since 0.11.0
--
data Tuple59 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2)

-- | A tuple of 60 elements.
--
-- @since 0.11.0
--
data Tuple60 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2)

-- | A tuple of 61 elements.
--
-- @since 0.11.0
--
data Tuple61 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2,i2)

-- | A tuple of 62 elements.
--
-- @since 0.11.0
--
data Tuple62 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2)

-- | A tuple of 63 elements.
--
-- @since 0.11.0
--
data Tuple63 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2)

-- | A tuple of 64 elements.
--
-- @since 0.11.0
--
data Tuple64 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2)
