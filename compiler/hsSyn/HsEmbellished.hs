{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module HsEmbellished (
  Embellished(..),
  LEmbellished,
  noEmb,
  unEmb,
  unLEmb,
  unLocEmb,
  lEmb,
  reEmb,
  reLEmb
  ) where

import SrcLoc
import Outputable

import Data.Data

-- | An embellished name
--
-- The parser can read a RdrName with either parens or backquotes around them.
-- This type wraps the name and captures whichever embellishment is present.
data Embellished name
  = EName       name
  | EParens     (Located name)
  | EBackquotes (Located name)
  deriving (Data, Ord, Eq, Functor, Foldable, Traversable)

type LEmbellished name = Located (Embellished name)

noEmb :: name -> LEmbellished name
noEmb n = noLoc $ EName n

unEmb :: Embellished name -> name
unEmb (EName            n)  = n
unEmb (EParens     (L _ n)) = n
unEmb (EBackquotes (L _ n)) = n

unLEmb :: LEmbellished name -> Located name
unLEmb (L l en) = L l (unEmb en)

unLocEmb :: LEmbellished name -> name
unLocEmb (L _ en) = unEmb en

lEmb :: Located name -> LEmbellished name
lEmb  (L l n) = L l $ EName n

reEmb :: Embellished name1 -> name2 -> Embellished name2
reEmb (EName _)             n = EName n
reEmb (EParens (L l _))     n = EParens (L l n)
reEmb (EBackquotes (L l _)) n = EBackquotes (L l n)

reLEmb :: LEmbellished name1 -> name2 -> LEmbellished name2
reLEmb (L l e) n = L l (reEmb e n)

instance (Outputable name) => Outputable (Embellished name) where
  pprPrec n en = pprPrec n (unEmb en)

instance (OutputableBndr name) => OutputableBndr (Embellished name) where
  pprPrefixOcc en = pprPrefixOcc (unEmb en)
  pprInfixOcc  en = pprInfixOcc  (unEmb en)
