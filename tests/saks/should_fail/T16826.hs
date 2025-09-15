{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module T16826 where

import Data.Kind

type family Id (x :: Type) :: Type where
  Id x = x

type C :: Type -> Id Constraint
class C a
