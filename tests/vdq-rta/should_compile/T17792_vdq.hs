{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module T17792_vdq where

class C a where
  m :: a

-- Check that instantiating an out-of-scope identifier with a
-- required type argument in a class instance and with deferred
-- type errors does not lead to a panic.
--
-- This problem was observed with TypeApplications in #17792.
-- Here we check that RequiredTypeArguments, a somewhat related
-- feature, is not affected.
instance C Bool where
  m = notInScope (type Word)
