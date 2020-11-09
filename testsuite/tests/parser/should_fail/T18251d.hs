{-# LANGUAGE ExplicitForAll #-}

module T18251d where

f :: forall a. a -> ()
f @a _ = ()
