{-# LANGUAGE NoImplicitPrelude #-}
module Endo where

-- This test threw up a WARNING (in a -DDEBUG compiler)
-- in GHC.Core.Opt.Arity.tryEtaReduce

newtype Endo a = Endo { appEndo :: a -> a }

foo :: Endo a -> Endo a -> Endo a
foo (Endo f) (Endo g) = Endo (comp f g)

comp :: (b -> c) -> (a -> b) -> (a -> c)
comp f g x = f (g x)
{-# OPAQUE comp #-}
