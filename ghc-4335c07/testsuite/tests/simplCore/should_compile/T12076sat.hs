{-# LANGUAGE MagicHash #-}
module T12076sat where

-- This test demonstrates that we need to saturate
-- primops even when they don't occur in function position.

import GHC.Exts

f = I# (dataToTag# timesWord#)
