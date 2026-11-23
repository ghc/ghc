{-# LANGUAGE NoExperimentalDependentTypes #-}

module NativeNamedOpaqueCallbackProvider (mapper, otherMapper) where

{-# INLINE mapper #-}
mapper :: Bool -> Char
mapper = error "forced unused callback"

otherMapper :: Bool -> Char
otherMapper input = if input then 'r' else error "forced unused input"
