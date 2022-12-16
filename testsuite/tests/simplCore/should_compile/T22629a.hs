module T22629 where

{-# INLINABLE foo #-}
{-# NOINLINE foo #-}
foo = case error "wombat" of { True -> "fred"; False -> "bill" }
