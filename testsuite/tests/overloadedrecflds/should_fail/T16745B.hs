{-# LANGUAGE DuplicateRecordFields #-}
-- This module tries to export a record field 'field' (defined below) and a
-- function 'field' (defined in another module), which shouldn't be allowed.
module T16745B
  ( R(field)
  , module T16745C
  ) where

import T16745C

data R = R { field :: Int}
