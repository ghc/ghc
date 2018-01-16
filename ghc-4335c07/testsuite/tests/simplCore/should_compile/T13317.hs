{-# LANGUAGE MagicHash #-}

module T13317 where

import GHC.Base

f x = let x = "foo"#
          y1 = unpackCString# x
          y2 = unpackCString# x
      in
      (y1, case y2 of
              'f' : _ -> True
              _       -> False
      )
-- This case-expression should simplify
-- yielding a KnownBranch simplifier tick
