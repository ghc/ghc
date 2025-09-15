{-# language UnboxedTuples #-}
module T14811 where

data Foo a = Foo (# a #)
data Bar = Bar (# #)
