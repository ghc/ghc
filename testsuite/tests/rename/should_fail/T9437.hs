{-# LANGUAGE RecordWildCards #-}

module T9437 where

data Foo = Foo { x :: Int }

test :: Foo -> Foo
test foo = foo { .. }
