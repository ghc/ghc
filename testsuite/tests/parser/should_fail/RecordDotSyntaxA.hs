{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, ScopedTypeVariables, PolyKinds, TypeApplications, DataKinds #-}
{-# LANGUAGE RecordWildCards, OverloadedRecordDot, OverloadedRecordUpdate #-}

module RecordDotSyntaxA where

class HasField x r a | x r -> a where
  hasField :: r -> (a -> r, a)

getField :: forall x r a . HasField x r a => r -> a
getField = snd . hasField @x -- Note: a.x = is getField @"x" a.

setField :: forall x r a . HasField x r a => r -> a -> r
setField = fst . hasField @x -- Note : a{x = b} is setField @"x" a b.

data Foo = Foo {foo :: Int}
instance HasField "foo" Foo Int where
    hasField r = (\x -> case r of Foo { .. } -> Foo { foo = x, .. }, foo r)
