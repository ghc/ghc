{-# LANGUAGE ExplicitForAll, TypeInType, GADTSyntax,
             ExistentialQuantification #-}

module T12911 where

import GHC.Exts

data X where
  MkX :: forall (a :: TYPE r). (a -> a) -> X
