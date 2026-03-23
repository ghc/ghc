{-# LANGUAGE NoImplicitPrelude #-}

-- This module is compiled with -hide-all-packages (see all.T), so neither
-- base nor GHC.Essentials can be found.  The numeric literal forces GHC to
-- look up the known-key 'fromInteger', which it tries to resolve via the
-- exports of GHC.Essentials.  Since GHC.Essentials cannot be found and
-- -frebindable-known-names is not in effect, this fails.
--
-- We want a structured, helpful error here rather than a panic; see
-- GHC.Iface.Load.loadKnownKeyOccMaps.
module T27013e where

n = 0
