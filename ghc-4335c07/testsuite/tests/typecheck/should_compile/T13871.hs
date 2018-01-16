{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module Foo where

import Data.Kind

data Foo (a :: Type) (b :: Type) where
  MkFoo :: (a ~ Int, b ~ Char) => Foo a b

data family Sing (a :: k)
data SFoo (z :: Foo a b) where
  SMkFoo :: SFoo MkFoo
