{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleInstances #-}
module ShouldFail where

import GHC.Exts( Constraint )

type family F a :: Constraint

class C a where

-- Should be rejected because of the type family
instance (F a) => C [[a]] where
