{-# LANGUAGE TypeAbstractions #-}

module T17594g where

id' :: forall {a}. a -> a
id' @a x = x
