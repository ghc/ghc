module T22629 where

-- This should not work as the activation here is not allowed.
{-# INLINABLE[1] foo #-}
{-# NOINLINE foo #-}
foo = case error "wombat" of { True -> "fred"; False -> "bill" }
