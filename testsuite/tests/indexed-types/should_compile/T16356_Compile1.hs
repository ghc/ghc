{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T16356_Compile1 where

import Data.Kind (Type)

data B (a :: k)

type family FClosed :: k -> Type where
  FClosed @k = B @k

type family FOpen :: k -> Type
type instance FOpen @k = B @k

class FAssocClass k where
  type FAssoc :: k -> Type

instance FAssocClass k where
  type FAssoc @k = B @k

class FAssocDefaultClass k where
  type FAssocDefault :: k -> Type
  type FAssocDefault @k = B @k
