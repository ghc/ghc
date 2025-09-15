{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Bug where

import Data.Kind

class C (a :: k) where
  type T k :: Type

-- This used to fail, with a mysterious error message
--    Type indexes must match class instance head
--      Expected: T (a1 -> Either a1 b1)
--      Actual: T (a -> Either a b)
-- but now it succeeds fine

instance C Left where
  type T (a -> Either a b) = Int
