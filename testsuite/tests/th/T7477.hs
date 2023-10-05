{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeFamilies, TemplateHaskell #-}

module T7477 where

import Language.Haskell.TH

type family F (a :: k)
type instance F Int = Bool

$( do { info <- reifyInstances ''F [ConT ''Int]
      ; reportWarning (pprint info)
      ; return [] })
