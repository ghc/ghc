{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}

module T7488 where

newtype A = A Bool
data B (x :: A)
