
module TH_dataD1 where

import Language.Haskell.TH

ds :: Q [Dec]
ds = [d|
          $(dataD [] (mkName "D") [] [normalC "K" []] [])
       |]

