{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies, NamedWildCards #-}
module NamedWildcardInTypeFamilyInstanceLHS where

type family F a where
  F _t = Int
