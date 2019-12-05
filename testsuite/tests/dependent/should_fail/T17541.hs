{-# LANGUAGE
             MagicHash,
             FlexibleInstances,
             MultiParamTypeClasses,
             TypeFamilies,
             PolyKinds,
             DataKinds,
             FunctionalDependencies,
             TypeFamilyDependencies #-}
module T17541 where

import GHC.Prim
import GHC.Exts


type family Rep rep where
  Rep Int = IntRep

type family Unboxed rep = (urep :: TYPE (Rep rep)) | urep -> rep where
  Unboxed Int = Int#
