{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module T21130 where

type family Item l
type instance Item [a] = a

f :: Enum (Item l) => l
f = f

x = (_ f) :: Int
