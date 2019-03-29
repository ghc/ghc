{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE GADTs #-}

module TLKS_Fail011 where

import Data.Kind (Type)

type G :: Type -> Type
data G where
  MkG :: a -> G a
