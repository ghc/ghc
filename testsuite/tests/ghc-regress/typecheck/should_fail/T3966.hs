{-# OPTIONS_GHC -Werror -O #-}
-- Add -O so the UNPACK has some effect

module T3966 where

data Foo a b = Foo {-# UNPACK #-} !(a -> b)
