
{-# LANGUAGE KindSignatures #-}

module Foo where

import Data.Kind (Type)

data Foo (a :: Type) = Foo a

