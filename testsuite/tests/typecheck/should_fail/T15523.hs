{-# LANGUAGE StrictData #-}

module T15523 where

newtype Duration = Foo
data Literal = LitDuration Duration
