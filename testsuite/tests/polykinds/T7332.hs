{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module T7332 where

import GHC.Exts( IsString(..) )
import Data.Monoid
import Data.Semigroup

newtype DC d = DC d
    deriving (Show, Semigroup, Monoid)

instance IsString (DC String) where
    fromString = DC


class Monoid acc => Build acc r where
    type BuildR r :: *          -- Result type
    build :: (acc -> BuildR r) -> acc -> r

instance Monoid dc => Build dc (DC dx) where
    type BuildR (DC dx) = DC dx
    build tr acc = tr acc

instance (Build dc r, a ~ dc) => Build dc (a->r) where
    type BuildR (a->r) = BuildR r
    build tr acc s = build tr (acc `mappend` s)


-- The type is inferred
-- tspan :: (Monoid d, Build (DC d) r, BuildR r ~ DC d) => r
tspan :: (Build (DC d) r, BuildR r ~ DC d) => r
tspan = build (id :: DC d -> DC d) mempty

{- Solving 'tspan'

Given:    Build (DC d) r, BuildR r ~ DC d
 (by sc)  Monoid (DC d)

   Wanted:
       Build acc0 r0
       Monid acc0
       acc0 ~ DC d0
       DC d0 ~ BuildR r0
       r ~ r0
==>
       Build (DC d0) r
       Monoid (DC d0)  -->  Monoid d0
       DC d0 ~ BuildR r

From Given: BuildR r = DC d, hence
       DC d0 ~ DC d
hence
       d0 ~ d

===>
       Build (DC d) r
       Monoid (DC d)

Now things are delicate.  Either the instance Monoid (DC d) will fire or,
if we are lucky, we might spot that (Monoid (DC d)) is a superclass of
a given.  But now (Decl 15) we add superclasses lazily, so that is less
likely to happen, and was always fragile.  So include (Monoid d) in the
signature, as was the case in the original ticket.
-}


foo = tspan "aa"

foo1 = tspan (tspan "aa")

bar = tspan "aa" :: DC String
