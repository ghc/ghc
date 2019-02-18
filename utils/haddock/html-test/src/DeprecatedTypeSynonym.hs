
module DeprecatedTypeSynonym where

-- | some documentation
type TypeSyn = String
{-# DEPRECATED TypeSyn "TypeSyn" #-}

type OtherTypeSyn = String
{-# DEPRECATED OtherTypeSyn "OtherTypeSyn" #-}
