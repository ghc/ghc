{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, ScopedTypeVariables, PolyKinds, TypeApplications, DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot, OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}
import Prelude

class HasField x r a | x r -> a where
  hasField :: r -> (a -> r, a)

getField :: forall x r a . HasField x r a => r -> a
getField = snd . hasField @x -- Note: a.x = is getField @"x" a.

setField :: forall x r a . HasField x r a => a -> r -> r
setField b a = fst (hasField @x a) b -- Note : a{x = b} is setField @"x" b a.

-- 'Foo' has 'foo' field of type 'Bar'
data Foo = Foo { foo :: Bar } deriving (Show, Eq)
instance HasField "foo" Foo Bar where
    hasField r = (\x -> case r of Foo { .. } -> Foo { foo = x, .. }, foo r)

-- 'Bar' has a 'bar' field of type 'Baz'
data Bar = Bar { bar :: Baz } deriving (Show, Eq)
instance HasField "bar" Bar Baz where
    hasField r = (\x -> case r of Bar { .. } -> Bar { bar = x, .. }, bar r)

-- 'Baz' has a 'baz' field of type 'Quux'
data Baz = Baz { baz :: Quux } deriving (Show, Eq)
instance HasField "baz" Baz Quux where
    hasField r = (\x -> case r of Baz { .. } -> Baz { baz = x, .. }, baz r)

-- 'Quux' has a 'quux' field of type 'Int'
data Quux = Quux { quux :: Int } deriving (Show, Eq)
instance HasField "quux" Quux Int where
    hasField r = (\x -> case r of Quux { .. } -> Quux { quux = x, .. }, quux r)

main = do
  let a = Foo { foo = Bar{ bar = Baz { baz = Quux { quux = 42 } } } }
  let quux = "Expecto patronum!"
  print $ a{foo.bar.baz.quux} -- Type error. Does a{foo.bar.baz.quux} get underlined?
