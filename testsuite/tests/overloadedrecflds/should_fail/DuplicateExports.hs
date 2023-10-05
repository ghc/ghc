{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Werror=duplicate-exports #-}

-- This should warn about the duplicate export of foo, but not the
-- exports of the two different bar fields.
module Export (T(foo, bar), foo, S(bar)) where

data T = MkT { foo :: Int, bar :: Int }
data S = MkS { bar :: Int }
