
{-# LANGUAGE DataKinds, StandaloneKindSignatures #-}

module RepPolyClassMethod where

import GHC.Exts

type C :: forall rep. TYPE rep -> Constraint
class C a where
  methC :: a
