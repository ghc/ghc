module T22629d where

import Data.List.NonEmpty as NE

import T22629d_Lib

-- getNumbers should get a specialization here.
-- As a result this while binding will optimize to just 42
-- so that's what the test checks for.

{-# NOINLINE foo #-}
foo = NE.head getNumbers :: Int

