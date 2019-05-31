{-# LANGUAGE RankNTypes, PolyKinds, GADTs, UndecidableSuperClasses, UndecidableInstances #-}
{-# LANGUAGE TopLevelKindSignatures #-}

module T11520 where

import Data.Kind (Type, Constraint)
import GHC.Types hiding (TyCon)

type TypeRep :: k -> Type
data TypeRep a

type Typeable :: k -> Constraint
class Typeable k => Typeable (a :: k) where
    typeRep :: TypeRep a

data Compose (f :: k1 -> *) (g :: k2 -> k1) (a :: k2) = Compose (f (g a))

-- Note how the kind signature on g is incorrect
instance (Typeable f, Typeable (g :: k), Typeable k) => Typeable (Compose f g) where
    typeRep = undefined
