{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeAbstractions #-}
module Bug where

import Data.Kind

type Const a b = a
data SameKind :: k -> k -> Type

type T :: forall a. Const Type a -> Type
newtype T @a k = MkT (forall (b :: k). SameKind a b)
