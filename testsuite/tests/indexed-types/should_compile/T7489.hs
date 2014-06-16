{-# LANGUAGE EmptyDataDecls, GADTs, TypeFamilies #-}

module T7489 where

data Credit
data Debit

data family Account (s :: *) (t :: *)

data instance Account Int t where
  CAccount :: Account Int Credit
  DAccount :: {  debitAccountPostings :: [Int] } -> Account Int Debit
