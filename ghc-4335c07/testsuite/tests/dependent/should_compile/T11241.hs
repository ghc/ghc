{-# LANGUAGE ExplicitForAll, KindSignatures, PartialTypeSignatures #-}

module T11241 where

foo :: forall (a :: _) . a -> a
foo = id
