{-# LANGUAGE TypeApplications, RankNTypes #-}

module ExplicitSpecificity1 where

foo :: forall {a}. a -> a
foo x = x

bar :: ()
bar = let x = foo @Int 42
      in ()
