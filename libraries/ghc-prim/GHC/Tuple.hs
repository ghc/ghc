{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS -ddump-tc-trace -ddump-to-file -ddump-file-prefix=GTfail #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Tuple
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/ghc-prim/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC extensions)
--
-- The tuple data types
--
-----------------------------------------------------------------------------

module GHC.Tuple where

import GHC.CString ()  -- Make sure we do it first, so that the
                       -- implicit Typeable stuff can see GHC.Types.TyCon
                       -- and unpackCString# etc

default () -- Double and Integer aren't available yet

-- | The unit datatype @Unit@ has one non-undefined member, the nullary
-- constructor @()@.
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

{-# DEPRECATED Solo "The Solo constructor has been renamed to MkSolo to avoid punning." #-}
pattern Solo :: a -> Solo a
pattern Solo x = MkSolo x
{-# COMPLETE Solo #-}

getSolo :: Solo a -> a
-- getSolo is a standalone function, rather than a record field of Solo,
-- because Solo is a wired-in TyCon, and a wired-in TyCon that  has
-- record fields is a bit more inconvenient than if it doesn't.
-- (No other wired-in TyCon has record fields.)  So it seems easier
-- to have getSolo as its own separate function (#20562)
getSolo (MkSolo a) = a

type Tuple0 = Unit
type Tuple1 = Solo

data Tuple2 a b = (a,b)
data Tuple3 a b c = (a,b,c)
data Tuple4 a b c d = (a,b,c,d)
data Tuple5 a b c d e = (a,b,c,d,e)
data Tuple6 a b c d e f = (a,b,c,d,e,f)
data Tuple7 a b c d e f g = (a,b,c,d,e,f,g)
data Tuple8 a b c d e f g h = (a,b,c,d,e,f,g,h)
data Tuple9 a b c d e f g h i = (a,b,c,d,e,f,g,h,i)
data Tuple10 a b c d e f g h i j = (a,b,c,d,e,f,g,h,i,j)
data Tuple11 a b c d e f g h i j k = (a,b,c,d,e,f,g,h,i,j,k)
data Tuple12 a b c d e f g h i j k l = (a,b,c,d,e,f,g,h,i,j,k,l)
data Tuple13 a b c d e f g h i j k l m = (a,b,c,d,e,f,g,h,i,j,k,l,m)
data Tuple14 a b c d e f g h i j k l m n = (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
data Tuple15 a b c d e f g h i j k l m n o = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
data Tuple16 a b c d e f g h i j k l m n o p = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
data Tuple17 a b c d e f g h i j k l m n o p q = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
data Tuple18 a b c d e f g h i j k l m n o p q r = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
data Tuple19 a b c d e f g h i j k l m n o p q r s = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
data Tuple20 a b c d e f g h i j k l m n o p q r s t = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
data Tuple21 a b c d e f g h i j k l m n o p q r s t u = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)
data Tuple22 a b c d e f g h i j k l m n o p q r s t u v = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)
data Tuple23 a b c d e f g h i j k l m n o p q r s t u v w = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w)
data Tuple24 a b c d e f g h i j k l m n o p q r s t u v w x = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
data Tuple25 a b c d e f g h i j k l m n o p q r s t u v w x y = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y)
data Tuple26 a b c d e f g h i j k l m n o p q r s t u v w x y z = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)

data Tuple27 a b c d e f g h i j k l m n o p q r s t u v w x y z a1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1)
data Tuple28 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1)
data Tuple29 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1)
data Tuple30 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1)
data Tuple31 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1)
data Tuple32 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1)
data Tuple33 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1)
data Tuple34 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1)
data Tuple35 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1)
data Tuple36 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1)
data Tuple37 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1)
data Tuple38 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1)
data Tuple39 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1)
data Tuple40 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1)
data Tuple41 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1)
data Tuple42 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1)
data Tuple43 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1)
data Tuple44 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1)
data Tuple45 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1)
data Tuple46 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1)
data Tuple47 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1)
data Tuple48 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1)
data Tuple49 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1)
data Tuple50 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1)
data Tuple51 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1)
data Tuple52 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1)
data Tuple53 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2)
data Tuple54 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2)
data Tuple55 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2)
data Tuple56 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2)
data Tuple57 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2)
data Tuple58 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2)
data Tuple59 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2)
data Tuple60 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2)
data Tuple61 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2,i2)
data Tuple62 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2)
data Tuple63 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2)
data Tuple64 a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
      r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,
     r1,s1,t1,u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2)
