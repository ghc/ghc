-- test representation and splicing of left-parenthesised right infix operators

module TH_spliceExpr1
where

import Language.Haskell.TH

foo :: Int
foo = $( [| ((+) $ 2) $ 2 |] )

