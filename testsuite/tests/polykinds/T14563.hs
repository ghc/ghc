{-# Language RankNTypes, KindSignatures, PolyKinds #-}
{-# OPTIONS_GHC -fprint-explicit-runtime-reps #-}

import GHC.Types (TYPE)
import Data.Kind

data Lan (g::TYPE rep -> TYPE rep') (h::TYPE rep -> TYPE rep'') a where
  Lan :: forall rep rep' xx (g::TYPE rep -> TYPE rep') (h::TYPE rep -> Type) a.
         (g xx -> a) -> h xx -> Lan g h a
