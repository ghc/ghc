{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wunused-binds #-}
module T13919 () where

import GHC.Generics

data Foo1 = Foo1 {bar1 :: String} deriving Show
data Foo2 = Foo2 {bar2 :: String} deriving Read
data Foo3 = Foo3 {bar3 :: String} deriving Generic

-- Only this one should emit a "Defined but not used" warning for its
-- record selector
data Foo4 = Foo4 {bar4 :: String} deriving Eq
