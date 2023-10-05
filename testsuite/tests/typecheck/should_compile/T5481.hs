{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module T5481 where

class Foo a b where
    type X a
    type X a = b
    type Y b
    type Y b = a
