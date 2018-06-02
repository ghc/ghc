module QuasiQuotation where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

stringify :: QuasiQuoter
stringify =  QuasiQuoter { quoteExp = return . LitE . StringL
                         , quotePat = return . LitP . StringL
                         , quoteType = return . LitT . StrTyLit
                         , quoteDec = const (return [])
                         }
