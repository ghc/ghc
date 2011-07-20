{-# LANGUAGE TypeFamilies #-}

module T4099 where

type family T a

foo :: T a -> Int
foo x = error "urk"

bar1 :: T b -> Int
bar1 x = foo x

bar2 :: Maybe b -> Int
bar2 x = foo x
