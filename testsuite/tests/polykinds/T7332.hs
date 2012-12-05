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
    type BuildR r :: *		-- Result type
    build :: (acc -> BuildR r) -> acc -> r

instance Monoid dc => Build dc (DC dx) where
    type BuildR (DC dx) = DC dx
    build tr acc = tr acc

instance (Build dc r, a ~ dc) => Build dc (a->r) where
    type BuildR (a->r) = BuildR r
    build tr acc s = build tr (acc `mappend` s)


-- The type is inferred
tspan :: (Monoid d, Build (DC d) r, BuildR r ~ DC d) => r
tspan = build (id :: DC d -> DC d) mempty

foo = tspan "aa"

foo1 = tspan (tspan "aa")

bar = tspan "aa" :: DC String
