{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T16110_Fail2 where

-- Ensure that kind variables don't leak into error messages if they're not
-- pertitent to the issue at hand
class C (a :: j) where
  type T (a :: j) (b :: k) (c :: k)
  type T a b b = Int
