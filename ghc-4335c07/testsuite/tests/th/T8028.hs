{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module T8028 where

import T8028a

import Language.Haskell.TH

$(x)

-- Check that the empty closed type family F produced by $(x) can
-- subsequently be reified
$(do f <- reify ''F
     case f of
       FamilyI (ClosedTypeFamilyD _ []) _ -> return []
       _ -> error $ show f
 )
