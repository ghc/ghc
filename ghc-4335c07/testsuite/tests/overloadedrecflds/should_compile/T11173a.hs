{-# LANGUAGE DuplicateRecordFields #-}
module T11173a where

data A = A { foo :: Int -> Int, bar :: Int -> Int }
newtype B = B { foo :: Int -> Int }
infixr 5 `foo`
infixr 5 `bar`

-- This is well-typed only if the fixity is correctly applied
y b = b `bar` b `bar` 0
