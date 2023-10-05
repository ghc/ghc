
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE LinearTypes #-}

module T21650_b where

import Data.Kind
import GHC.Exts


data T1

type RR :: Type -> Type -> RuntimeRep
type family RR t1 t2 where
  RR T1 _ = IntRep

type C1 :: Type -> Constraint
class    C1 t
instance C1 T1

type C2 :: Type -> Constraint
class C2 t


type N :: forall t1 t2 -> TYPE (RR t1 t2) -> TYPE (RR t1 t2)
newtype (C1 t1, C2 t2) => N t1 t2 a = MkN a

foo :: forall t2 (a :: TYPE (RR T1 t2)). C2 t2 => a -> N T1 t2 a
foo = MkN

bar :: forall t2 (a :: TYPE (RR T1 t2)). C2 t2 => a %1 -> N T1 t2 a
bar = MkN
