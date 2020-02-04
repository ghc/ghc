{-# LANGUAGE GADTs #-}

module T17648 where

data T a where
  A :: T Int
  B :: T Bool

f :: T Int -> ()
f A = ()
