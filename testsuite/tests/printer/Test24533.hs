{-# OPTIONS -ddump-parsed-ast #-}
module Test24533 where

instance
  ( Read a, -- Weird
    Read b
  ) =>
  Read (a, b)
