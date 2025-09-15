{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
module T16326_Fail12 where

class (forall a -> Show a) => C a
