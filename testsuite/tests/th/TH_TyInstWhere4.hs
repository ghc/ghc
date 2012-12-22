{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies #-}

module TH_TyInstWhere4 where

import Language.Haskell.TH

type family F a b :: Bool
type instance where
  F a a = True
  F a b = False

$( do { info1 <- reify ''F
      ; reportWarning (pprint info1)
      ; info2 <- reifyInstances ''F [ConT ''Int, ConT ''Int]
      ; reportWarning (pprint info2)
      ; info3 <- reifyInstances ''F [ConT ''Int, ConT ''Bool]
      ; reportWarning (pprint info3)
      ; return [] })


