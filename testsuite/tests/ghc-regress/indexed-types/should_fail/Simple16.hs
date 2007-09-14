{-# LANGUAGE TypeFamilies #-}

module ShouldFail where

type family F a

foo :: p a -> p a
foo x = x

bar = foo (undefined :: F ())

