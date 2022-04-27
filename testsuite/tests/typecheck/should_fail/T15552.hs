{-# LANGUAGE DataKinds, ExistentialQuantification, GADTs, PolyKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module T15552 where

import Data.Kind

data Elem :: k -> [k] -> Type

data EntryOfVal (v :: Type) (kvs :: [Type]) where
  EntryOfVal :: forall (v :: Type) (kvs :: [Type]) (k :: Type).
                Elem (k, v) kvs -> EntryOfVal v kvs

type family EntryOfValKey (eov :: EntryOfVal v kvs) :: Type

type family GetEntryOfVal (eov :: EntryOfVal v kvs) :: Elem (EntryOfValKey eov, v) kvs

type family FirstEntryOfVal (v :: Type) (kvs :: [Type]) :: EntryOfVal v kvs where
