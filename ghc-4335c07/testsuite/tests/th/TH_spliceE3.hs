-- test the representation of literals and also explicit type annotations

module TH_repE1
where

import Language.Haskell.TH

$( do let emptyListExpr :: ExpQ
          emptyListExpr = [| [] |]

          singletonListExpr :: ExpQ
          singletonListExpr = [| [4] |]

          listExpr :: ExpQ
          listExpr = [| [4,5,6] |]

          consExpr :: ExpQ
          consExpr = [| 4:5:6:[] |]

      [d| foo = ($emptyListExpr, $singletonListExpr, $listExpr, $consExpr) |]
 )

bar = $( [| case undefined of
                [1] -> 1 |] )

