{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module T18170c where

import Language.Haskell.TH.Lib

import GHC.Exts
import GHC.Types

import Unsafe.Coerce

-- Representation-polymorphism should be allowed here
-- as long as 'r1' is monomorphised at splice site.
repPolyApp
  :: forall r1 r2 (a :: TYPE r1) (b :: TYPE r2)
  . CodeQ ( (a -> b) -> a -> b )
repPolyApp = [|| \ f x -> f x ||]

