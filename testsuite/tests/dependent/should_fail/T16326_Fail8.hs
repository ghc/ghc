{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T16326_Fail8 where

class C a
data Blah a
instance forall a -> C (Blah a)
