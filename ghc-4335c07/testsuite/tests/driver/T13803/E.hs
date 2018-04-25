{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module E where

import                GHC.Exts (Constraint)
import {-# SOURCE #-} Y

data E

type family CF a :: * -> Constraint
type instance CF E = Y
