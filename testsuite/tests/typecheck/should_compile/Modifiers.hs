{-# OPTIONS_GHC -Wunknown-modifiers #-}
{-# LANGUAGE LinearTypes, RequiredTypeArguments, DataKinds #-}

module Modifiers where

import GHC.Types (Multiplicity(..))

%() data A
  = %() A1 Int
  | %True A2 String
  | %False A3 { a3 %() :: () }

%() data A' where
  %() A1' :: Int -> A'
  %True A2' :: String -> A'
  %False A3' :: { a3' %() :: () } -> A'

%() %True
class B a

%()
%True
instance B A

l1 :: forall (m :: Bool) a b . a %m -> b
l1 = undefined

l2 :: forall (m :: Multiplicity) a b . a %m -> b
l2 = undefined

l3 :: a %(m :: Multiplicity) -> b %(m :: Multiplicity) -> c
l3 = undefined

-- MODS_TODO these are expected to fail, not tested in this test case:
-- l4 :: a %m -> b
-- l5 :: a %(m :: Multiplicity) -> b %m -> c

idt :: forall t -> t -> t
idt _ x = x

visForallApp :: Int -> Int
visForallApp = idt (Int %() -> Int) (+ 1)

-- Which type variables get exposed to modifiers?
--
-- MODS_TODO the current proposal doesn't make it clear what the results should
-- be here. The commented out ones fail to compile, but that's not currently
-- tested.

%a data FV1 a

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
