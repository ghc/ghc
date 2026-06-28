-- Exercises -dcanonicalize-local-binds on an attached specialisation RULE.
-- The SPECIALISE pragma's rule has an internal dictionary binder, canonicalized
-- in its own fresh scope (see canonIdInfo / Note [Canonicalizing local binders
-- for dumps] in GHC.Core.Canonicalize).
module T27176c (spec) where

spec :: Num a => a -> a -> a
spec x y = x * y + x
{-# SPECIALISE spec :: Int -> Int -> Int #-}
