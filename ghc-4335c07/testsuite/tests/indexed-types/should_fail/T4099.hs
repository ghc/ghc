{-# LANGUAGE TypeFamilies #-}

module T4099 where

type family T a

foo :: a -> T a -> Int
foo x = error "urk"

bar1 :: b -> T b -> Int
bar1 a x = foo (error "urk") x

bar2 :: b -> Maybe b -> Int
bar2 a x = foo (error "urk") x
