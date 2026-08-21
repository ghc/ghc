-- The two branches share an identical suffix from the inner case
-- onwards, so common block elimination merges the duplicated call
-- blocks over several rounds, building a substitution chain. Without
-- resolving that chain, compiling this module at -O2 panicked in
-- setInfoTableStackMap (#27368). See
-- Note [Resolving the CBE substitution] in GHC.Cmm.CommonBlockElim.

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
