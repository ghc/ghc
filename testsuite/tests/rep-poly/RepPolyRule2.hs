{-# LANGUAGE DataKinds, PolyKinds, StandaloneKindSignatures, TypeFamilyDependencies #-}

module RepPolyRule2 where

import GHC.Exts

type F :: RuntimeRep -> RuntimeRep
type family F rep  = r | r -> rep where
  F IntRep  = WordRep
  F WordRep = IntRep
  F rep     = rep

f :: forall rep (a :: TYPE (F rep)). a -> a
f = undefined

{-# NOINLINE f #-}
{-# RULES "f_id" forall (x :: (a :: TYPE (F rep))). f x = x #-}
