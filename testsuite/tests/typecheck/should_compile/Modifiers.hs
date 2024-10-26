{-# OPTIONS_GHC -Wunknown-modifiers #-}
{-# LANGUAGE LinearTypes, RequiredTypeArguments, DataKinds #-}

module Modifiers where

import GHC.Types (Multiplicity(..))

%() data A
  = %() A1 Int
  | %True A2 String
  | %False A3 { a3 %() :: () }

%() data A_Gadt where
  %() A1_Gadt :: Int -> A_Gadt
  %True A2_Gadt :: String -> A_Gadt
  %False A3_Gadt :: { a3Gadt %() :: () } -> A_Gadt

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
