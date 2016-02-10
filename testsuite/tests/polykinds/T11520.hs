{-# LANGUAGE RankNTypes, PolyKinds, TypeInType, GADTs, UndecidableSuperClasses #-}

module T11520 where

import GHC.Types hiding (TyCon)

data TypeRep (a :: k)

class Typeable k => Typeable (a :: k) where
    typeRep :: TypeRep a

data Compose (f :: k1 -> *) (g :: k2 -> k1) (a :: k2) = Compose (f (g a))

-- Note how the kind signature on g is incorrect
instance (Typeable f, Typeable (g :: k), Typeable k) => Typeable (Compose f g) where
    typeRep = undefined
