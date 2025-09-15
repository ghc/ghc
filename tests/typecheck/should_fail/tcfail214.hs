{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
module ShouldFail where

import GHC.Exts( Constraint )

type family F a :: Constraint

class C a where
instance (F a) => C [a] where
