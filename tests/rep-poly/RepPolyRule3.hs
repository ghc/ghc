{-# LANGUAGE DataKinds, PolyKinds, StandaloneKindSignatures, TypeFamilyDependencies #-}

module RepPolyRule3 where

import GHC.Exts

type F :: RuntimeRep -> RuntimeRep
type family F rep  = r | r -> rep where
  F IntRep  = WordRep
  F WordRep = IntRep
  F rep     = rep

g :: forall rep (a :: TYPE (F rep)). a -> a
g = undefined

{-# NOINLINE g #-}
{-# RULES "g_id" forall (x :: (a :: TYPE (F WordRep))). g x = x #-}

h :: forall rep (a :: TYPE (F rep)). a -> a
h = undefined

{-# NOINLINE h #-}
{-# RULES "h_id" forall (x :: (a :: TYPE IntRep)). h x = x #-}
