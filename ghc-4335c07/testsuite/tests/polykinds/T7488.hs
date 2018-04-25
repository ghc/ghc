{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE GADTs              #-}

module T7488 where

newtype A = A Bool
data B (x :: A)
