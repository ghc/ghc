{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies, TypeApplications #-}

module TH_TyInstWhere2 where

import Language.Haskell.TH hiding (Type)
import Data.Kind

$( do { decs <- [d| type family F (a :: k) (b :: k) :: Bool where
                      F a a = True
                      F a b = False |]
      ; reportWarning (pprint decs)
      ; return [] })

$( do { dec1 <- [d| type family F1 (a :: k) :: Type where
                      F1 @Type Int = Bool
                      F1 @Bool 'False = Char |]
      ; reportWarning (pprint dec1)
      ; return [] })
