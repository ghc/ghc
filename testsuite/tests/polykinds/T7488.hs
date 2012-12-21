{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE GADTs              #-}

module T7488 where

newtype A = A Int
data B (x :: A)
