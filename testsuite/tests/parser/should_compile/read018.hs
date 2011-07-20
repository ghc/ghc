{-# LANGUAGE DatatypeContexts #-}
-- !!! Checking that empty contexts are permitted.
module ShouldCompile where

data () => Foo a = Foo a

newtype () => Bar = Bar Int

f :: () => Int -> Int
f = (+1)


class () => Fob a where

instance () => Fob Int where
instance () => Fob Float

