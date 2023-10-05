{-# LANGUAGE ExistentialQuantification, LambdaCase, LinearTypes #-}

module T19400 where

data Stream = forall s. Stream (Either s s)

f :: x %m -> y %m -> Int
f x y = f x y

step' :: () -> Stream -> Int
step' x (Stream s) =
  (\case
    Left  y -> f x y
    Right y -> f x y) s
