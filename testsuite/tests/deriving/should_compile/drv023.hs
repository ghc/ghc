-- deriving for data instances is supposed to work

{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeFamilies #-}

module ShouldSucceed where

import Data.Ix
import Data.Kind
import Data.Traversable
import Data.Data
import Language.Haskell.TH.Syntax (Lift)
import GHC.Generics

data family Foo :: Type -> Type -> Type

data instance Foo () a = Foo1 | Foo2 | Foo3 | Foo4 | Foo5 | Foo6 | Foo7
  deriving (Eq, Ord, Read, Show, Enum, Ix, Functor, Foldable, Traversable, Data, Lift, Generic)

data instance Foo [x] y = T1 | T2 | T3 | T4 x | T5 | T6 | T7 | T8 | T9 y y | T10 ([y], x)
  deriving (Eq, Ord, Read, Show,           Functor, Foldable, Traversable, Data, Lift, Generic)

data instance Foo (Maybe z) Int = Z (Int, z) z
  deriving (Eq, Ord, Read, Show,       Ix,                                 Data, Lift, Generic)
