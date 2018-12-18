{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
module VisibleDependentQuantificationFail12 where

class (forall a -> Show a) => C a
