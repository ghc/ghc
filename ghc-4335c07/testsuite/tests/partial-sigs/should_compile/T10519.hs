{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PartialTypeSignatures #-}
module T10519 where

foo :: forall a. _ => a -> a -> Bool
foo x y = x == y
