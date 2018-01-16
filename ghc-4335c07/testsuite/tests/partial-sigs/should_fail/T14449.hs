{-# LANGUAGE PartialTypeSignatures #-}

module T14449 where

f :: a -> b -> _
f x y = [x, y]
