{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UnboxedTuples #-}
-- A regression test for #24750. This test ensures that a family of functions
-- from the `template-haskell` library (tupeTypeName, tupleDataName, etc.)
-- returns the same Names as when you manually quote the names using
-- TemplateHaskellQuotes.
module Main (main) where

import Control.Monad (unless)
import GHC.Tuple (Tuple2)
import GHC.Types (Sum2#, Tuple2#)
import Language.Haskell.TH

test :: Name -> Name -> IO ()
test n1 n2 =
  unless (n1 == n2) $
    fail $ unlines
      [ "Names are not equal"
      , "LHS name: " ++ show n1
      , "RHS name: " ++ show n2
      ]

main :: IO ()
main = do
  test (tupleTypeName 2) ''(,)
  test (tupleTypeName 2) ''Tuple2
  test (tupleDataName 2) '(,)
  test (unboxedTupleTypeName 2) ''(#,#)
  test (unboxedTupleTypeName 2) ''Tuple2#
  test (unboxedTupleDataName 2) '(#,#)
  test (unboxedSumTypeName 2) ''Sum2#
  -- There is currently no way to manually quote an unboxed sum data constructor
  -- Name, as you cannot write unboxed sum data constructors in prefix form. As
  -- such, a test case for `unboxedSumDataName` is omitted.
