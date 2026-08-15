-- Regression test for #27368: setInfoTableStackMap panicked because
-- callProcPoints collected the continuation of a call in an unreachable
-- block. The two branches below have identical suffixes from the inner
-- case onwards. Common block elimination merges the duplicated call
-- blocks, leaving the losing copies in the block map. Since
-- replaceLabels does not resolve the resulting substitution chains, one
-- dead call ends up returning to a label that no reachable block
-- mentions.
module T27368 (f) where

{-# NOINLINE put #-}
put :: Int -> Int -> IO ()
put h x = if h + x == 12345 then errorWithoutStackTrace "boom" else pure ()

data T = N | J Int | K

f :: Int -> Bool -> T -> IO ()
f h a t = do
  if a
    then do put h 1; case t of { N -> pure (); J _ -> put h 3; K -> put h 4 }; put h 0; put h 0
    else do put h 2; case t of { N -> pure (); J _ -> put h 3; K -> put h 4 }; put h 0; put h 0
  put h 0
