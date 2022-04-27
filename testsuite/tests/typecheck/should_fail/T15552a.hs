{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds, ExistentialQuantification, GADTs, PolyKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-  # LANGUAGE UndecidableInstances #-}
module T15552 where

import Data.Kind

data Elem :: k -> [k] -> Type where
  Here :: Elem x (x : xs)
  There :: Elem x xs -> Elem x (y : xs)

data EntryOfVal (v :: Type) (kvs :: [Type]) where
  EntryOfVal :: forall (v :: Type) (kvs :: [Type]) (k :: Type).
                Elem (k, v) kvs -> EntryOfVal v kvs

type family EntryOfValKey (eov :: EntryOfVal v kvs) :: Type where
  EntryOfValKey ('EntryOfVal (_ :: Elem (k, v) kvs)) = k

type family GetEntryOfVal (eov :: EntryOfVal v kvs)
            :: Elem (EntryOfValKey eov, v) kvs
  where
    GetEntryOfVal ('EntryOfVal e) = e

type family FirstEntryOfVal (v :: Type) (kvs :: [Type]) :: EntryOfVal v kvs
  where FirstEntryOfVal v (_ : kvs)
           = 'EntryOfVal (There (GetEntryOfVal (FirstEntryOfVal v kvs)))
--type instance FirstEntryOfVal v (_ : kvs)
--         = 'EntryOfVal ('There (GetEntryOfVal (FirstEntryOfVal v kvs)))
