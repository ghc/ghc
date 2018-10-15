{-# LANGUAGE ExplicitForAll, PolyKinds, GADTSyntax,
             ExistentialQuantification #-}

module T12911 where

import GHC.Exts

data X where
  MkX :: forall r (a :: TYPE r). (a -> a) -> X
