-- Test selectors cannot be used ambiguously

{-# LANGUAGE DuplicateRecordFields #-}

data R = MkR { x :: Int, y :: Bool }
data S = MkS { x :: Int }

main = do print (x (MkS 42))
          print (y (MkR 42 42))
