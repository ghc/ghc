{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, PolyKinds #-}
module NativeRetainedCastDomainReject where

import Data.Coerce (coerce)
import ParameterizedIndexLibrary

data Holder (token :: DomainToken tag) = Holder

-- The phantom parameter permits an ordinary representational coercion.
representational :: SourceToken -> DomainToken Char
representational = coerce

bad :: (foreach (x :: SourceToken) -> Holder x)
    -> foreach (y :: DomainToken Char) -> Holder y
bad original = original
