{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, ScopedTypeVariables, PolyKinds, TypeApplications, DataKinds #-}
{-# LANGUAGE OverloadedRecordDot, OverloadedRecordUpdate #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
import Prelude

class HasField x r a | x r -> a where
  hasField :: r -> (a -> r, a)

getField :: forall x r a . HasField x r a => r -> a
getField = snd . hasField @x -- Note: a.x = is getField @"x" a.

setField :: forall x r a . HasField x r a => a -> r -> r
setField b a = fst (hasField @x a) b -- Note : a{x = b} is setField @"x" b a.

-- 'Foo' has 'foo' field of type 'Int'
data Foo = Foo { foo :: Int } deriving (Show, Eq)
instance HasField "foo" Foo Int where
    hasField r = (\x -> case r of Foo { .. } -> Foo { foo = x, .. }, foo r)
main = do
  let a = Foo {foo = 12};
  -- let foo = 13;
  print $ a {foo}
