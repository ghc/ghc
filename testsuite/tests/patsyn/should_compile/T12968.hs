{-# LANGUAGE PolyKinds , GADTs, ScopedTypeVariables, PatternSynonyms,
      ViewPatterns #-}

module T12968 where

data TypeRep (a :: k)

data TRAppG (fun :: k2) where
  TRAppG :: forall k1 k2 (a :: k1 -> k2) (b :: k1) .
            TypeRep a -> TypeRep b -> TRAppG (a b)

pattern TRApp :: forall k2 (fun :: k2). ()
              => forall k1 (a :: k1 -> k2) (b :: k1). (fun ~ a b)
              => TypeRep a -> TypeRep b -> TypeRep fun
pattern TRApp a b <- ((undefined :: TypeRep fun -> TRAppG fun) -> TRAppG a b)
