{-# LANGUAGE TypeFamilies #-}

module T9318 where

type family F x
type instance F Int = Bool

foo :: F Int -> ()
foo True = ()

bar :: F Int -> ()
bar 'x' = ()
