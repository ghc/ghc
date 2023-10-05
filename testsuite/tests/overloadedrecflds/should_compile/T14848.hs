{-# language TemplateHaskell #-}
{-# language DuplicateRecordFields #-}

module T14848 where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data A = A {x :: Int, y :: String}
a = A 3 "test"
test = $([e|case a of A {x = b} -> b|])
