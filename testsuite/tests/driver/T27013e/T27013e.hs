{-# LANGUAGE NoImplicitPrelude #-}

-- This module is compiled with -hide-all-packages (see all.T), so neither
-- base nor GHC.Essentials can be found.  Without -frebindable-known-names GHC
-- resolves known entities through GHC.Essentials, and so implicitly imports it;
-- that import cannot be resolved.
--
-- We want a structured, helpful error here rather than a panic; see
-- Note [Finding GHC.Essentials] in GHC.Builtin.
module T27013e where

n = 0
