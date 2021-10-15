{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module T18170b where

import Language.Haskell.TH.Lib

import T18170c

import GHC.Exts
import GHC.Types

-- Representation-polymorphic function application: should be rejected.
unsound :: forall r1 r2 (a :: TYPE r1) (b :: TYPE r2)
        . (a -> b) -> a -> b
unsound = $$repPolyApp
