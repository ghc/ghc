{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitNamespaces #-}

module T25901_sub_w2_ok where

import Data.Bool (Bool(False, data ..))

true :: Bool
true = True     -- ok: imported via subordinate wildcard `data ..`

false :: Bool
false = False   -- ok: imported via explicit subordinate item `False`
