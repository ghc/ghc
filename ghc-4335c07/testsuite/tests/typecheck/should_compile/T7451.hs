module T6117a where

-- Tests that Lint does not complain about
-- a mis-match between * and Constraint

class B a where
     b :: a

newtype Additive a = Additive a

instance B a => B (Additive a) where
    b = Additive b
