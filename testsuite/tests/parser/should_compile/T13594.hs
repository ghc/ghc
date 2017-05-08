{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Bug where

x :: forall a b. (a ~ Integer, b ~ Integer) => (a, b)
!x = (1, 2)
