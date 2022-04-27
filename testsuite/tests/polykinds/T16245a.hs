{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Bug where

import Data.Kind

type Const a b = a
data SameKind :: k -> k -> Type

newtype T (k :: Const Type a) = MkT (forall (b :: k). SameKind a b)
