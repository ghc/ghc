{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -frebindable-known-names #-}
module NativeClosedResponseNames where

import Data.Type.Equality ((:~:)(Refl))
import qualified Data.Maybe

-- The compiler must supply the interpreter response primitives itself.
result :: Data.Maybe.listToMaybe '[1,2] :~: 'Just 1
result = Refl
