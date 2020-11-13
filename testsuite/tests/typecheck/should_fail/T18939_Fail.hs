{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
module T18939_Fail where

data F (f :: forall a -> a)
