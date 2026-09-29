-- Common block elimination merges the duplicated blocks over several
-- rounds. Compiling this at -O panicked in setInfoTableStackMap (#27368).

module T27368 (f) where

{-# NOINLINE put #-}
put :: Int -> Int -> IO ()
put h x = if h + x == 12345 then errorWithoutStackTrace "boom" else pure ()

data T = J Int | K

f :: Int -> Bool -> T -> IO ()
f h a t = do
  if a
    then do put h 1; case t of { J _ -> put h 3; K -> put h 4 }; put h 0; put h 0
    else do put h 2; case t of { J _ -> put h 3; K -> put h 4 }; put h 0; put h 0
  put h 0
