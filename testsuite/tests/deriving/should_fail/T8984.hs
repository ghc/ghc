{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving #-}
module T8984 where

class C a where
    app :: a (a Int)

newtype N cat a b = MkN (cat a b)  deriving( C )
-- The newtype coercion is   N cat ~R cat
