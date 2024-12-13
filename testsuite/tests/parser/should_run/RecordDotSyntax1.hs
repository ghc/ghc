{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, ScopedTypeVariables, PolyKinds, TypeApplications, DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot, OverloadedRecordUpdate #-}
-- For "higher kinded data" test.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE RebindableSyntax #-}
import Prelude

-- Choice (C2a).

import Data.Function -- for &
import Data.Functor.Identity

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

-- 'Corge' has a '&&&' field of type 'Int'
data Corge = Corge { (&&&) :: Int } deriving (Show, Eq)
instance HasField "&&&" Corge Int where
    hasField r = (\x -> case r of Corge { .. } -> Corge { (&&&) = x, .. }, (&&&) r)
-- Note : Dot notation is not available for fields with operator
-- names.

-- 'Grault' has two fields 'f' and 'g' of type 'Foo'.
data Grault = Grault {f :: Foo, g :: Foo} deriving (Show, Eq)
instance HasField "f" Grault Foo where
    hasField r = (\x -> case r of Grault { .. } -> Grault { f = x, .. }, f r)
instance HasField "g" Grault Foo where
    hasField r = (\x -> case r of Grault { .. } -> Grault { g = x, .. }, g r)

-- "Higher kinded data"
-- (see https://reasonablypolymorphic.com/blog/higher-kinded-data/)
type family H f a where
  H Identity a = a
  H f        a = f a
data P f = P
  { n :: H f String
  }
-- See https://github.com/ndmitchell/record-dot-preprocessor/pull/34.
instance (a ~ H f String) => HasField "n" (P f) a where
    hasField r = (\x -> case r of P { .. } -> P { n = x, .. }, n r)

main = do
  let a = Foo { foo = Bar{ bar = Baz { baz = Quux { quux = 42 } } } }
  let b = Corge{ (&&&) = 12 };
  let c = Grault {
        f = Foo { foo = Bar{ bar = Baz { baz = Quux { quux = 1 } } } }
      , g = Foo { foo = Bar{ bar = Baz { baz = Quux { quux = 1 } } } }
       }

  -- A "selector" is an expression like '(.a)' or '(.a.b)'.
  putStrLn "-- selectors:"
  print $ (.foo) a  -- Bar { bar = Baz { baz = Quux { quux = 42 } } }
  print $ (.foo.bar) a -- Baz { baz = Quux { quux = 42 } }
  print $ (.foo.bar.baz) a -- Quux { quux = 42 }
  print $ (.foo.bar.baz.quux) a -- 42
  print $ ((&&&) b) -- 12
  -- print $ (b.(&&&)) -- illegal : parse error on input ‘(’
  print $ getField @"&&&" b -- 12

  -- A "selection" is an expression like 'r.a' or '(f r).a.b'.
  putStrLn "-- selections:"
  print $ a.foo.bar.baz.quux -- 42
  print $ a.foo.bar.baz -- Quux { quux = 42 }
  print $ a.foo.bar -- Baz { baz = Quux { quux = 42 } }
  print $ a.foo -- Bar { bar = Baz { baz = Quux { quux = 42 } } }
  print $ (const "hello") a.foo  -- f r.x means f (r.x)
  -- print $ f a .foo  -- f r .x is illegal
  print $ (const "hello") (id a).foo  -- f (g r).x means f ((g r).x)
  -- print $ f (g a) .foo -- f (g r) .x is illegal
  print $ a.foo
            & (.bar.baz.quux) -- 42
  print $ (a.foo
               ).bar.baz.quux -- 42
  print $ (+) a.foo.bar.baz.quux 1 -- 43
  print $ (+) (id a).foo.bar.baz.quux 1 -- 43
  print $ (+) ((id a).foo.bar & (.baz.quux)) 1 -- 43

  -- An "update" is an expression like 'r{ a.b = 12 }'.
  putStrLn "-- updates:"
  print $ (a.foo.bar.baz) { quux = 2 } -- Quux { quux = 2 }
  print $ (\b -> b{ bar=Baz{ baz=Quux{ quux=1 } } }) a.foo -- Bar { bar = Baz { baz = Quux { quux = 1 } } }
  let bar = Bar { bar = Baz { baz = Quux { quux = 44 } } }
  print $ a{ foo.bar = Baz { baz = Quux { quux = 44 } } } -- Foo { foo = Bar { bar = Baz { baz = Quux { quux = 44 } } } }
  print $ a{ foo.bar.baz = Quux { quux = 45 } } -- Foo { foo = Bar { bar = Baz { baz = Quux { quux = 45 } } } }
  print $ a{ foo.bar.baz.quux = 46 } -- Foo { foo = Bar { bar = Baz { baz = Quux { quux = 46 } } } }
  print $ c{ f.foo.bar.baz.quux = 3, g.foo.bar.baz.quux = 4 } -- Grault { f = Foo { foo = Bar { bar = Baz { baz = Quux { quux = 3 } } } }, g = Foo { foo = Bar { bar = Baz { baz = Quux { quux = 4 } } } } }

  -- A "punned update" is an expression like 'r{ a.b }' (where it is
  -- understood that 'b' is a variable binding in the environment of
  -- the field update - enabled only when the extension
  -- 'NamedFieldPuns' is in effect).
  putStrLn "-- punned updates:"
  let quux = 102; baz = Quux { quux }; bar = Baz { baz }; foo = Bar { bar } -- Foo { foo = Bar { bar = Baz { baz = Quux { quux = 102 } } } }
  print $ a{ foo.bar.baz.quux } -- Foo { foo = Bar { bar = Baz { baz = Quux { quux = 102 } } } }
  print $ a{ foo.bar.baz } -- Foo { foo = Bar { bar = Baz { baz = Quux { quux = 102 } } } }
  print $ a{ foo.bar } -- Foo { foo = Bar { bar = Baz { baz = Quux { quux = 102 } } } }
  print $ a{ foo } -- Foo { foo = Bar { bar = Baz { baz = Quux { quux = 102 } } } }
  print $ a -- Foo { foo = Bar { bar = Baz { baz = Quux { quux = 42 } } } }
  print $ c{ f.foo, g.foo.bar.baz.quux = 4 } -- Mix punned and explicit; 102, 4
  f <- pure a
  g <- pure a
  print $ c{ f } -- 42, 1
  print $ c{ f, g } -- 42, 42
  print $ c{ f, g.foo.bar.baz.quux = 4 } -- Mix top-level and nested updates; 42, 4

  putStrLn "-- misc:"
  -- Higher kinded test.
  let p = P { n = Just "me" } :: P Maybe
  Just me <- pure p.n
  putStrLn $ me
