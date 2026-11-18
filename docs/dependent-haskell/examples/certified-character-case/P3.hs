{-# LANGUAGE DataKinds, FlexibleContexts, NoExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies #-}
module P3 (p3) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownChar, charVal)
import Upper (Upper)

p3 :: forall c uc. (Upper c ~ uc, KnownChar uc) => Proxy c -> Char
p3 Proxy = charVal (Proxy @uc)
