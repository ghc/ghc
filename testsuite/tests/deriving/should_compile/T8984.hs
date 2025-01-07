{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving #-}
module T8984 where

class C a where
    app :: a (a Int)

newtype N cat a b = MkN (cat a b)  deriving( C )
-- The inferred instance is:
--
--   instance
--     (Coercible (cat a (N cat a Int)) (cat a (cat a Int)), C (cat a)) =>
--     C (N cat a)
