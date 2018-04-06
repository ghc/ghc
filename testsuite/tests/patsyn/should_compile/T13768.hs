{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module T13768 where

data NS (f :: k -> *) (xs :: [k]) = NS Int

data IsNS (f :: k -> *) (xs :: [k]) where
  IsZ :: f x -> IsNS f (x ': xs)
  IsS :: NS f xs -> IsNS f (x ': xs)

isNS :: NS f xs -> IsNS f xs
isNS = undefined

pattern Z :: () => (xs' ~ (x ': xs)) => f x -> NS f xs'
pattern Z x <- (isNS -> IsZ x)

pattern S :: () => (xs' ~ (x ': xs)) => NS f xs -> NS f xs'
pattern S p <- (isNS -> IsS p)

{-# COMPLETE Z, S #-}

data SList :: [k] -> * where
  SNil  :: SList '[]
  SCons :: SList (x ': xs)

go :: SList ys -> NS f ys -> Int
go SCons (Z _) = 0
go SCons (S _) = 1
go SNil  _     = error "inaccessible"
