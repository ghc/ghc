{-# Language RankNTypes #-}
{-# Language DataKinds  #-}
{-# Language PolyKinds  #-}
{-# Language GADTs      #-}
module T15872 where

import Data.Kind

data WHICH = OP | OPOP

data Fun :: forall (a :: WHICH). a ~ OP => Type -> Type -> Type where
  MkFun :: (a -> b) -> Fun a b
