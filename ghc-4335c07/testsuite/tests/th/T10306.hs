{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module T10306 where

import Language.Haskell.TH
import GHC.TypeLits

-- Attempting to reify a built-in type family like (+) previously
-- caused a crash, because it has no equations
$(do x <- reify ''(+)
     case x of
       FamilyI (ClosedTypeFamilyD _ []) _ -> return []
       _ -> error $ show x
 )
