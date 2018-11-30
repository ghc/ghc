{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, GADTs, FlexibleInstances #-}

module T14045a where

import Data.Kind

class C (a :: k) where
  data S (a :: k)

-- This used to fail with the mysterious error
--   Type indexes must match class instance head
--     Expected: S z
--     Actual: S a
-- But now it is fine
instance C (z :: Bool) where
  data S :: Bool -> Type where
    SF :: S False
    ST :: S True
