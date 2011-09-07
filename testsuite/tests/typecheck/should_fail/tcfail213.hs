{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
module ShouldFail where

type family F a :: Constraint

class (F a) => C a where
