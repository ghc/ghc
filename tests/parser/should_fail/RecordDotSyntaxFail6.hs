{-# LANGUAGE OverloadedRecordDot, OverloadedRecordUpdate, RebindableSyntax #-}

module Main where

import qualified RecordDotSyntaxA as A

main = do
  let bar = A.Foo { A.foo =1 } -- A defn. Perfectly reasonable.
  print $ A.foo bar -- Application of a selector. Also reasonable.
  let baz = bar{A.foo = 2} -- An update with a qualified field; not supported!
  print $ A.foo baz
