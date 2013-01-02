{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies #-}

module TH_TyInstWhere2 where

import Language.Haskell.TH

type family F (a :: k) (b :: k) :: Bool

$( do { decs <- [d| type instance where
                      F a a = True
                      F a b = False |]
      ; reportWarning (pprint decs)
      ; return [] })


