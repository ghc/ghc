{-# LANGUAGE ScopedTypeVariables, TypeAbstractions #-}

module ImplicitPunUses where

a :: Integer
a = 15

implicit :: a -> a
implicit x = x

explicit :: forall a. a -> a
explicit x = x

nested :: t -> t
nested @a = \(a :: a) -> a

localCapture :: t -> s -> t
localCapture q (_ :: q) = q
