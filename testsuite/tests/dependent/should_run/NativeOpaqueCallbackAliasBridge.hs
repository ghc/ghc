{-# LANGUAGE NoExperimentalDependentTypes #-}

module NativeOpaqueCallbackAliasBridge (bridge) where

import NativeOpaqueCallbackAliasProof (accepts, token)

{-# INLINE bridge #-}
bridge :: (Bool -> Char) -> Bool
bridge f = accepts f True (token f True)
