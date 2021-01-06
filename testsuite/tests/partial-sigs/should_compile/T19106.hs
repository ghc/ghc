{-# LANGUAGE TypeFamilies, GADTs, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module T19106 where

f :: (a ~ [b]) => T a -> _ -> String
f (MkT x) _ = show x

data T a where
  MkT :: G a => a -> T a

type family G a where
  G [b] = Show b
