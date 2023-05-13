{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module DCo_Typeable where

import GHC.Prim ( TYPE )
import GHC.Base ( Type, RuntimeRep(BoxedRep), Levity(Lifted), undefined )

splitApp :: TypeRep a -> AppOrCon a
splitApp TrType = IsApp trTYPE trLiftedRep
  where
    trTYPE :: TypeRep TYPE
    trTYPE = undefined
    trLiftedRep :: TypeRep ('BoxedRep 'Lifted)
    trLiftedRep = undefined

type TypeRep :: k -> Type
data TypeRep (a :: k) where
  TrType :: TypeRep Type

data AppOrCon (a :: k) where
  IsApp :: forall k k' (f :: k' -> k) (x :: k'). ()
        => TypeRep f %1 -> TypeRep x %1 -> AppOrCon (f x)
