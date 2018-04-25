{-# LANGUAGE TypeFamilies #-}

module Simple10 where

type family T a

foo, bar :: T a -> a
foo = undefined
bar x = foo x

