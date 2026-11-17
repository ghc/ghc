{-# LANGUAGE GADTs, TypeFamilies #-}

-- GHC 9.14.1 panics with "updateRole" when compiling this module.
-- Compiles fine with GHC 9.12 (but by a fluke)
module T28135 where

data S t where
  W :: C f => (forall (a :: k). S (t a)) -> S (t f)

class C f
