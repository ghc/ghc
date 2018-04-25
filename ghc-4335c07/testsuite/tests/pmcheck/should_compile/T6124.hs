{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module T6124 where

newtype A = MkA Int
newtype B = MkB Char

data T a where
    A :: T A
    B :: T B

f :: T A -> A
f A = undefined
