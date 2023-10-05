{-# LANGUAGE TypeFamilies #-}

module T6123 where

type family Id a

cid :: a ~ Id a => a -> a
cid x = x

cundefined = cid undefined
