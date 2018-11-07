{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module E where

import                Data.Kind (Type, Constraint)
import {-# SOURCE #-} Y

data E

type family CF a :: Type -> Constraint
type instance CF E = Y
