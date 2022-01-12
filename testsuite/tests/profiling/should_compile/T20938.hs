{-# language MagicHash #-}

-- We can't put code ticks between things which are required to be saturated
-- and their arguments.
module T20938 where

import GHC.Exts

foo x = ({-# SCC foo #-} tagToEnum#) x :: Bool

