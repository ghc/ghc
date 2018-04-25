{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module T13487 where

import Data.Kind (Constraint)
import GHC.TypeLits

data Foo a b where
  K :: Error a b => a -> b -> Foo a b

type family Error a b :: Constraint where
  Error Int Int = ()
  Error _   _   = TypeError ('Text "GHC panic in 3... 2... 1...")

foo = K 'a' 'b'
