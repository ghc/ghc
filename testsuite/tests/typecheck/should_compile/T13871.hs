{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TopLevelKindSignatures #-}
module Foo where

import Data.Kind

data Foo (a :: Type) (b :: Type) where
  MkFoo :: (a ~ Int, b ~ Char) => Foo a b

data family Sing (a :: k)

type SFoo :: Foo a b -> Type
data SFoo z where
  SMkFoo :: SFoo MkFoo
