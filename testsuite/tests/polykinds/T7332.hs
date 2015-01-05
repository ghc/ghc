{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module T7332 where

import GHC.Exts( IsString(..) )
import Data.Monoid

newtype DC d = DC d
    deriving (Show, Monoid)

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

{- Wanted:
       Build acc0 r0
       Monid acc0
       acc0 ~ DC d0
       DC d0 ~ BuildR r0
==>
       Build (DC d0) r0
       Monoid (DC d0)  -->  Monoid d0
       DC d- ~ BuildR r0

In fact Monoid (DC d0) is a superclass of (Build (DC do) r0)
But during inference we do not take upserclasses of wanteds
-}


foo = tspan "aa"

foo1 = tspan (tspan "aa")

bar = tspan "aa" :: DC String
