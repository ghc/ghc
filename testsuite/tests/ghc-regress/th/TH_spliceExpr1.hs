-- test reification of type synonym declarations

module TH_spliceExpr1
where

import Language.Haskell.THSyntax

foo :: Int
foo = $( [| ((+) $ 2) $ 2 |] )

