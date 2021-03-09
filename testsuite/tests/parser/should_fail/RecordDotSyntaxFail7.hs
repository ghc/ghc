{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified RecordDotSyntaxA as A

main = do
  let bar = A.Foo { A.foo =1 } -- A defn. Perfectly reasonable.
  print $ (bar.A.foo) -- A field selection where the field is qualified; parse error on input ‘A.foo’.
