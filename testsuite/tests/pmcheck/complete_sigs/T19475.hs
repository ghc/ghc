{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module T19475 where

class C f where
  foo :: f a -> ()
pattern P :: C f => f a
pattern P <- (foo -> ())
{-# COMPLETE P #-}

class D f where
  bar :: f a -> ()
pattern Q :: D f => f a
pattern Q <- (bar -> ())

g :: D f => f a -> ()
g Q = () -- Warning should not suggest P!
