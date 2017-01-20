{-# LANGUAGE ScopedTypeVariables #-}
module T12441 where
import T12441A
f :: forall b a. (a, b)
f = undefined
