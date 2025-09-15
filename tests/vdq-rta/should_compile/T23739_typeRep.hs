{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module T23739_typeRep where

import Type.Reflection
import Data.Kind
import GHC.TypeLits

typeRepVis :: forall a -> Typeable a => TypeRep a
typeRepVis t = typeRep @t

typeRepVis_lam :: forall a -> Typeable a => TypeRep a
typeRepVis_lam = \t -> typeRep @t

typeRepVis_kforall :: forall k. forall (a :: k) -> Typeable a => TypeRep a
typeRepVis_kforall = typeRepVis

typeRepKiVis :: forall k (a :: k) -> Typeable a => TypeRep a
typeRepKiVis k (a :: k) = typeRep @a