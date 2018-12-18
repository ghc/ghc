{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module T12503 where

import Language.Haskell.TH

data T1 k
class C1 a

$(do TyConI (DataD [] tName [ KindedTV kName kKind] _ _ _)
       <- reify ''T1
     d <- instanceD (cxt [])
                    (conT ''C1 `appT`
                      (conT tName `appT` sigT (varT kName) kKind))
                    []
     return [d])

data family T2 (a :: b)
data instance T2 b
class C2 a

$(do FamilyI (DataFamilyD tName _ _) [DataInstD [] _ tyVar _ _ _]
       <- reify ''T2
     d <- instanceD (cxt [])
                    (conT ''C2 `appT` return tyVar)
                    []
     return [d])
