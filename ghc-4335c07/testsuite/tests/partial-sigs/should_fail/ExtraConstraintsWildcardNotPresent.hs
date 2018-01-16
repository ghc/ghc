{-# LANGUAGE PartialTypeSignatures #-}
module ExtraConstraintsWildcardNotPresent where


show' :: a -> _
show' x = show x

-- With an extra-constraints wildcard present, this would lead to the
-- type Show a => a -> String.

-- This test makes sure that not merely having a partial type
-- signature is enough to generate extra constraints, an
-- extra-constraints wildcard is needed.
