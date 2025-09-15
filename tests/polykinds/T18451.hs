{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Bug where

import Data.Kind

type Const a b = a
data SameKind :: k -> k -> Type

type T (k :: Const Type a) = forall (b :: k). SameKind a b
