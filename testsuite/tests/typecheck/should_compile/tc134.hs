{-# LANGUAGE ScopedTypeVariables #-}

-- !!! Scoped type variables: result sig

module Test where

f :: Int -> Int
f x :: Int = x

g :: Int -> Int
g x :: a = x :: a	-- Here, a is a name for Int
