{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module VisibleDependentQuantificationFail8 where

class C a
data Blah a
instance forall a -> C (Blah a)
