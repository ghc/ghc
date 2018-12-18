{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module VisibleDependentQuantificationFail10 where

import Data.Kind

type Const a b = a

flurmp :: a -> ()
flurmp _ = ()
{-# RULES "flurmp"
    forall (x :: forall a -> a -> a). flurmp x = () #-}
