module T16976z where

import Language.Haskell.TH

do  let str :: String
        str = "defined inside the splice"
    t <- reifyType 'str
    return []
