{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module DerivingViaFail where

import Control.Category
import Data.Functor.Identity

newtype Foo1 a = Foo1 a deriving Show via (Identity b)

newtype Foo2 a b = Foo2 (a -> b)
  deriving Category
    via fooo

data Foo3 deriving Eq via (forall a. a)

newtype Foo4 a = Foo4 a
deriving via (Identity b)
  instance Show (Foo4 a)
