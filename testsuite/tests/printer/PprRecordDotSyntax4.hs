{-# LANGUAGE OverloadedRecordDot #-}

module PprRecordDotSyntax4 where

import qualified RecordDotSyntaxA as A

main = do
  let bar = A.Foo {A.foo = 1}
  print $ bar{A.foo = 2} -- Qualified labels ok in regular updates.
