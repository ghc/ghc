{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T16326_Fail10 where

import Data.Kind

type Const a b = a

flurmp :: a -> ()
flurmp _ = ()
{-# RULES "flurmp"
    forall (x :: forall a -> a -> a). flurmp x = () #-}
