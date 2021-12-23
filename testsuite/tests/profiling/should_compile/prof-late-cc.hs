{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Test where

import GHC.Prim
import GHC.Types

-- This caused problems during implementation.
-- See #20938
foo x = tagToEnum# x :: Bool

bar x = ({-# SCC scc_bar #-} tagToEnum#) x :: Bool
