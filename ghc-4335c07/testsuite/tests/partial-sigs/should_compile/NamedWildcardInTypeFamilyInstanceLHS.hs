{-# LANGUAGE TypeFamilies, NamedWildCards #-}
module NamedWildcardInTypeFamilyInstanceLHS where

type family F a where
  F _t = Int
