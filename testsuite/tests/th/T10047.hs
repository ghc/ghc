module T10047 where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

n = QuasiQuoter { quoteExp = dyn }
