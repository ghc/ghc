module T13473a where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

quoter :: QuasiQuoter
quoter = QuasiQuoter { quotePat  = varP . mkName
                     , quoteExp  = undefined
                     , quoteDec  = undefined
                     , quoteType = undefined }
