{-# LANGUAGE NoExperimentalDependentTypes #-}

module NativeOpaqueCallbackAliasProvider (mapper, otherMapper) where

{-# OPAQUE mapper #-}
mapper :: Bool -> Char
mapper x = if x then 'l' else error "mapper demanded its input"

{-# OPAQUE otherMapper #-}
otherMapper :: Bool -> Char
otherMapper x = if x then 'r' else error "otherMapper demanded its input"
