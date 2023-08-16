{-# LANGUAGE TypeAbstractions #-}
module T17594c where

id' :: forall a. [a]
id' = [\ @t -> undefined :: t]
