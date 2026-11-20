{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
module NativeTypeCaseFallbackRigid where

import Data.Proxy
import Data.Type.Equality
import NativeTypeCaseFallback

unknown :: Proxy b -> Force b :~: Int
unknown _ = Refl
