{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, FlexibleContexts, GADTs, Modifiers, PolyKinds, TypeOperators #-}
module MappingScope where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import MappingTags (Tagged(..))
import MappingWorker (MapLike(..))

-- | This instance exchanges the tags of its input and output.
tag :: forall a b (f :: a %U -> b) (x :: a). Proxy f -> Proxy x
#ifdef WRONG_TAG
    -> mapLike f ('Old x) :~: 'Old (f x)
#else
    -> mapLike f ('Old x) :~: 'New (f x)
#endif
tag _ _ = Refl

-- | A local dictionary leaves the implementation of this method open.
present :: forall a b (f :: a %U -> b) (x :: a).
#ifdef GIVEN
           MapLike Maybe =>
#endif
           Proxy f -> Proxy x -> mapLike f ('Just x) :~: 'Just (f x)
present _ _ = Refl
