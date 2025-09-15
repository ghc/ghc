module T13688Quasi where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

aQuoter :: QuasiQuoter
aQuoter =
    QuasiQuoter { quotePat  = return . LitP . StringL
                , quoteExp  = return . LitE . StringL
                , quoteType = undefined
                , quoteDec  = undefined
                }
