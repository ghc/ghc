{-# LANGUAGE NoImplicitPrelude #-}

-- Check we get a reasonable error when defining a custom GHC.Essentials
-- that does not define everything we need.
module GHC.Essentials where

data Q = Q1 | Q2
