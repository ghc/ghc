{-# LANGUAGE TypeAbstractions #-}

module T22478c where


import Data.Kind (Type, Constraint)
import GHC.Exts (Multiplicity(Many))

data T (a :: Type) = MkT


fShadowing :: T a -> T b -> T a
fShadowing (MkT @a) = \(MkT @a) -> MkT @a
