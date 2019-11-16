module T16976f where

import Language.Haskell.TH

do  t <- reifyType (mkName "doesn'tExist")
    return []
