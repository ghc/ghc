{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, KindSignatures #-}
{-# LANGUAGE PolyKinds, ScopedTypeVariables, TypeFamilies #-}
module NativeTypeLambdaFormation (formed) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))

data At (k :: Type) (tag :: k) = At
type family Tag (k :: Type) (tag :: k) :: Type where
  Tag k tag = At k tag

formed :: forall k tag. Proxy (tag :: k) -> Bool
formed _ = go (Proxy :: Proxy Char)
  where
#ifdef SHADOW
    go :: forall (k :: Type). Proxy k -> Bool
#else
    go :: forall (j :: Type). Proxy j -> Bool
#endif
    go _ = const True witness
      where
#if defined(CONSTRUCTOR)
        witness :: Proxy (\cell -> (At k tag, cell))
#elif defined(FAMILY)
        witness :: Proxy (\cell -> (Tag k tag, cell))
#else
        witness :: Proxy (\cell -> (Proxy (tag :: k), cell))
#endif
        witness = Proxy
