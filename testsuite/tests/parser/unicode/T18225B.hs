{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module T18225B where

f :: (a, (b, c)) -> b
f (_, (x, _)) = x

test :: a -> a
test = proc x -> ⦇f⦈$([|x|])
