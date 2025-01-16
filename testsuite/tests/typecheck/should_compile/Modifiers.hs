{-# OPTIONS_GHC -Wunknown-modifiers #-}
{-# LANGUAGE LinearTypes, RequiredTypeArguments, DataKinds, TypeData #-}

module Modifiers where

import GHC.Types (Multiplicity(..), Type)

%()
data D
  = %() D1 Int
  | %True D2 String
  | %False D3 { d3 %() :: () }
  | %Bool Int :* Bool
  | (%Maybe Int) :** Bool

%()
data D' where
  %() D1' :: Int -> D'
  %True D2' :: String -> D'
  %False D3' :: { d3' %() :: () } -> D'

%()
newtype N = %True N { n %False :: Int }

%()
newtype N' where
  %True N' :: { n' %False :: Int } -> N'

%()
type data TD
  = %() TD1 Int
  | %True TD2 String

%()
type data TD' where
  %() TD1' :: Int -> TD'
  %True TD2' :: String -> TD'

%() %True
class C a

%()
%True
instance C D

%()
default (Int)

%True
foreign import ccall "test" someImport :: Int
%True
foreign export ccall someImport :: Int

%()
sigDecl :: ()
sigDecl = undefined

l1 :: forall (m :: Bool) a b . a %m -> b
l1 = undefined

l2 :: forall (m :: Multiplicity) a b . a %m -> b
l2 = undefined

l3 :: a %(m :: Multiplicity) -> b %(m :: Multiplicity) -> c
l3 = undefined

l4 :: a %() -> b
l4 = undefined

-- MODS_TODO This one should probably fail, since Just :: Maybe a is poly-kinded
l5 :: a %Just -> b
l5 = undefined

-- MODS_TODO it's kinda weird that this one works but l8 fails
l6 :: a %m -> b %(m :: Multiplicity) -> c
l6 = undefined

-- MODS_TODO these are expected to fail, not tested in this test case:
-- l7 :: a %m -> b
-- l8 :: a %(m :: Multiplicity) -> b %m -> c

t1 :: %'() Int
t1 = 0

t2 :: Maybe (%True Int)
t2 = Nothing

t3 :: Maybe (%(%True True) Int)
t3 = Nothing

idt :: forall t -> t -> t
idt _ x = x

visForallApp :: Int -> Int
visForallApp = idt (Int %() -> Int) (+ 1)

-- Which type variables get exposed to modifiers?
--
-- MODS_TODO the current proposal doesn't make it clear what the results should
-- be here. The commented out ones fail to compile, but that's not currently
-- tested.
--
-- Note that for some of these, the kind of the type variable isn't fixed. So if
-- these are allowed, rename-only modifiers with unknown kind are in fact
-- possible.

%a
data FV1 a

data FV2 a = %a FV2

-- data FV2' a where
--   %a FV2' :: FV2'

data FV3 a = FV3 { fv3 %a :: () }

-- data FV3' a where
--   FV3' :: { fv3' %a :: () } -> FV3

-- MODS_TODO this compiles, but seems especially sketchy.
data FV4 = %a forall a . FV4 a

data FV4' where
  %a FV4' :: a -> FV4'

-- data FV5 = FV5 { fv5 %a :: forall a . a }

-- %a class FV6 a

-- %a instance B a

fv7 :: forall (a :: Type) . %a a
fv7 = undefined

fv8 :: %(a :: Type) a
fv8 = undefined

-- fv9 :: %a a -- unknown kind
-- fv9 = undefined

-- And which concrete types?

%CT1
data CT1

%CT2Con
data CT2 = CT2Con

%CT2Con'
data CT2' where
  CT2Con' :: CT2'

%CT3
class CT3 a
