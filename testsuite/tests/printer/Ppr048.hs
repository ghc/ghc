module Ppr048 where

{-# SCc foo #-}
foo :: Int -> Int
foo       x = x

{-# SCc foo2 "label" #-}
foo2 :: ()
foo2 = ()
