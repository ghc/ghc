{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language DeriveGeneric #-}

module T8479 where

import GHC.Generics

class Blah (a :: * -> *) where
  type F a :: * -> *

data Foo (f :: * -> *) a = MkFoo ((F f) a) deriving Generic1
