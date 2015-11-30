-- Test purpose:
-- Ensure that missing semigroup warnings are issued
-- correctly if the warning flag is enabled

{-# OPTIONS_GHC -fwarn-semigroup #-}

module SemigroupWarnings where



import Data.Semigroup



-- Bad instance, should complain about missing Semigroup parent
data LacksSemigroup
instance Monoid LacksSemigroup where
    mempty = undefined
    mappend = undefined



-- Correct instance, should not warn
data HasSemigroup
instance Semigroup HasSemigroup where
    (<>) = undefined
instance Monoid HasSemigroup where
    mempty = undefined
    mappend = undefined



-- Should issue a Prelude clash warning
(<>) = undefined
