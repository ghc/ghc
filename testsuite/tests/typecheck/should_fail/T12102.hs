{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module T12102 where

import Data.Kind
import GHC.TypeLits

type family IsTypeLit a where
  IsTypeLit Nat    = 'True
  IsTypeLit Symbol = 'True
  IsTypeLit a      = 'False

data T :: forall a. (IsTypeLit a ~ 'True) => a -> Type where
  MkNat    :: T 42
  MkSymbol :: T "Don't panic!"
