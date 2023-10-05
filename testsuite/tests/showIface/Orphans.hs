{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -O -Wno-inline-rule-shadowing #-}
-- Rules are ignored without -O

module Orphans where

import GHC.Exts (IsList(..))

-- Some orphan things
instance IsList Bool where
  type Item Bool = Double
  fromList = undefined
  toList = undefined

{-# RULES "myrule1" id id = id #-}

-- And some non-orphan things
data X = X [Int]
instance IsList X where
  type Item X = Int
  fromList = undefined
  toList = undefined

f :: X -> X
f x = x
{-# RULES "myrule2" id f = f #-}
