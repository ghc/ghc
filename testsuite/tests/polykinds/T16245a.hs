{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
module Bug where

import Data.Kind

type Const a b = a
data SameKind :: k -> k -> Type

newtype T (k :: Const Type a) = MkT (forall (b :: k). SameKind a b)
