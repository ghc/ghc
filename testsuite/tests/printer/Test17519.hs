{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Test17519 where

type family Proxy2' ∷ ∀ k → k → Type where
  Proxy2' = Proxy'
