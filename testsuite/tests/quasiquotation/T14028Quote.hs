module T14028Quote where
import Language.Haskell.TH
import Language.Haskell.TH.Quote

here :: QuasiQuoter
here = QuasiQuoter { quoteExp  = litE . stringL,
                     quotePat = undefined ,
                     quoteType = undefined,
                     quoteDec = undefined }
