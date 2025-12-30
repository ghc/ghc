{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitNamespaces #-}

module T25901_sub_w2_fail where

import Data.Bool (Bool(False, type ..))

true :: Bool
true = True     -- error: /not/ imported via subordinate wildcard `type ..`

false :: Bool
false = False   -- ok: imported via explicit subordinate item `False`
