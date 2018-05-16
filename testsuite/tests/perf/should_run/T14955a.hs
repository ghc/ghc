{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module T14955a where

import Prelude (Bool(..), (||), (&&))

-- Implementation 1

class Prop r where
  or :: r -> r -> r
  and :: r -> r -> r
  true :: r
  false :: r

instance Prop Bool where
  or = (||)
  and = (&&)
  true = True
  false = False

-- Implementation 2

data PropDict r = PropDict {
  dor :: r -> r -> r
  , dand :: r -> r -> r
  , dtrue :: r
  , dfalse :: r
  }

boolDict = PropDict {
  dor = (||)
  , dand = (&&)
  , dtrue = True
  , dfalse = False }

-- Implementation 3

class PropProxy r where
  propDict :: PropDict r

instance PropProxy Bool where
  propDict = boolDict

-- Implementation 4

class PropProxy2 r where
  propDict2 :: PropDict r
  dummy :: ()

instance PropProxy2 Bool where
  propDict2 = boolDict
  dummy = ()


ors :: Prop r => [r] -> r
ors [] = true
ors (o:os) = o `or` ors os
{-# INLINABLE ors #-}

dors :: PropDict r -> [r] -> r
dors pd [] = dtrue pd
dors pd (o:os) = dor pd o (dors pd os)

pors :: PropProxy r => [r] -> r
pors [] = dtrue propDict
pors (o:os) = dor propDict o (pors os)
{-# INLINABLE pors #-}

porsProxy :: PropProxy2 r => [r] -> r
porsProxy [] = dtrue propDict2
porsProxy (o:os) = dor propDict2 o (porsProxy os)
{-# INLINABLE porsProxy #-}
