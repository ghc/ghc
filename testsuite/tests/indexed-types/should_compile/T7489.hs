{-# LANGUAGE EmptyDataDecls, GADTs, TypeFamilies #-}

module T7489 where

import Data.Kind (Type)

data Credit
data Debit

data family Account (s :: Type) (t :: Type)

data instance Account Int t where
  CAccount :: Account Int Credit
  DAccount :: {  debitAccountPostings :: [Int] } -> Account Int Debit
