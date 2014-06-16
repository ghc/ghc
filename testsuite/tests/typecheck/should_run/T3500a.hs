{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

type family F a :: *
type instance F Int = (Int, ())

class C a
instance C ()
instance (C (F a), C b) => C (a, b)

f :: C (F a) => a -> Int
f _ = 2

main :: IO ()
main = print (f (3 :: Int))

