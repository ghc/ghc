{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T16110_Fail3 where

import Data.Kind

-- Ensure that kind variables don't leak into error messages if they're not
-- pertitent to the issue at hand
class C (a :: j) where
  type T (a :: j) (b :: Type)
  type T a Int = Int
