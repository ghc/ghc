{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
module ShouldFail where

type family F a :: Constraint

class C a where
instance (F a) => C [a] where
