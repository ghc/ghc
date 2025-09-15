{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Bug where

import Data.Kind

data SameKind :: forall k. k -> k -> Type
data Foo :: forall a k (b :: k). SameKind a b -> Type where
  MkFoo :: Foo sameKind
