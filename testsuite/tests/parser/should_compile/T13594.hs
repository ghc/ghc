{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Bug where

x :: forall a . a ~ Integer => forall b. b ~ Integer => (a, b)
!x = (1, 2)
