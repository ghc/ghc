{-# LANGUAGE GADTs, ConstraintKinds, Rank2Types, ImplicitParams #-}

module T12507 where

data Rec fields where
  Rec :: fields => Rec fields

qn :: Rec fields -> (fields => r) -> r
qn Rec e = e

record :: Rec (?a :: Int, ?b :: String)
record = Rec where ?a=42
                   ?b="hey"

access :: Int
access = qn record ?a
