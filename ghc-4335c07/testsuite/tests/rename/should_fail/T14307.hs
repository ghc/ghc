{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module T14307 where

data A = A { field :: Int }
data B = B { field :: Int }

f :: B -> Int
f (C { field }) = field
