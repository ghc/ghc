{-# OPTIONS -fglasgow-exts #-}

-- This one killed GHC 5.05 and earlier
-- The problem was in a newtype with a record selector, with
-- a polymorphic argument type.  MkId generated a bogus selector
-- function

module ShouldCompile where

  type M3 a = forall r. (forall b. M3' b -> (b -> M3' a) -> r) -> r

  newtype M3' a
      = M3' { mkM3' :: M3 a }

  bind :: M3 a -> (a -> M3 b) -> M3 b
  bind m k b = b (M3' m) (\a -> M3' (k a))

  observe :: M3 a -> a
  observe m
      = m (\m' k -> mkM3' m'
                (\bm k1 -> observe (bind (mkM3' bm)
                        (\a -> bind (mkM3' (k1 a)) (\a -> mkM3' (k a)))))
                )

