{-# LANGUAGE TypeFamilies #-}
module UnsatisfiableWarning where

data Foo a

foo :: (Foo a ~ a)
    => Foo a -> a
foo = undefined
