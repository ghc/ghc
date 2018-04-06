{-# Language PatternSynonyms, ViewPatterns, GADTs, ConstraintKinds, RankNTypes, KindSignatures, PolyKinds, ScopedTypeVariables, DataKinds #-}

module T14498 where

import Type.Reflection
import Data.Kind

data Dict c where Dict :: c => Dict c

asTypeable :: TypeRep a -> Dict (Typeable a)
asTypeable rep =
  withTypeable rep
    Dict

pattern Typeable :: () => Typeable a => TypeRep a
pattern Typeable <- (asTypeable -> Dict)
  where Typeable = typeRep

data N = O | S N

type SN = (TypeRep :: N -> Type)

pattern SO = (Typeable :: TypeRep (O::N))

pattern SS ::
     forall (t :: k').
     ()
  => forall (a :: kk -> k') (n :: kk).
     (t ~ a n)
  =>
  TypeRep n -> TypeRep t
pattern SS n <- (App (Typeable :: TypeRep (a ::kk -> k')) n)
