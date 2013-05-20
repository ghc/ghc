
module TH_dataD1 where

import Language.Haskell.TH

ds :: Q [Dec]
ds = [d|
          $(do { d <- dataD (cxt []) (mkName "D") [] [normalC (mkName "K") []] []
               ; return [d]})
       |]

