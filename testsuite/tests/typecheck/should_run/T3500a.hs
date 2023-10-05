{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Kind (Type)

type family F a :: Type
type instance F Int = (Int, ())

class C a
instance C ()
instance (C (F a), C b) => C (a, b)

f :: C (F a) => a -> Int
f _ = 2

main :: IO ()
main = print (f (3 :: Int))

