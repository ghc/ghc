{-# LANGUAGE PartialTypeSignatures #-}
module Foo where

f :: (Show a) => a -> _ -> Bool
f x y = x>x
