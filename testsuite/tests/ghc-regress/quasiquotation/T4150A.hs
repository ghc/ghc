
module T4150A where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

multiLineStr :: QuasiQuoter
multiLineStr = QuasiQuoter {
                   quoteExp  = stringE,
                   quotePat  = error "XXX",
                   quoteType = error "XXX",
                   quoteDec  = error "XXX"
               }
