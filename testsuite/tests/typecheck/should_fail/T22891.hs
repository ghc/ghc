{-# LANGUAGE UndecidableInstances #-}

module T22891 where

class Foo f

class Foo f => Bar f g

instance Bar f f => Bar f (h k)
