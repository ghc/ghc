{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module T9750 where

import Data.Kind (Type)
import GHC.TypeLits ( Symbol, KnownSymbol )

--------------------------------------------------------------------------------

data Meta = MetaCons Symbol
data M1 (c :: Meta) = M1

class Generic a where
  type Rep a :: Type
  from  :: a -> Rep a

--------------------------------------------------------------------------------

data A = A1

instance Generic A where
  type Rep A = M1 ('MetaCons "test")
  from A1 = M1

class GShow' f where
  gshowsPrec' :: f -> ShowS

instance (KnownSymbol c) => GShow' (M1 ('MetaCons c)) where
  gshowsPrec' = error "urk"

instance GShow' A where
  gshowsPrec' = gshowsPrec' . from
