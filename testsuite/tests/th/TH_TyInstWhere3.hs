{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies #-}

module TH_TyInstWhere3 where

import Language.Haskell.TH

type family F a

$( do { decs <- [d| type instance where
                      F Int = Int |]
      ; reportWarning (pprint decs)
      ; return decs })

type instance F a = a 

-- When this test was written, TH considered all singleton type family instance
-- as unbranched. Thus, even though the two instances above would not play nicely
-- without TH, they should be fine with TH.
