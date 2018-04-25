module A where

-- Reproduces an issue where rules would abstract over typeclass dictionaries
-- non-deterministically.
--
-- Compare:
--
-- RULES: "SPECLOL $csize" [ALWAYS]
--            forall ($dOrd_a1sc :: Ord Int) ($dNum_a1sd :: Num Int).
--              $csize_a1sg @ Int $dOrd_a1sc $dNum_a1sd
--              = $s$csize_d1zr]
-- with:
--
-- RULES: "SPEC $csize" [ALWAYS]
--            forall ($dNum_a18n42 :: Num Int) ($dOrd_a18n43 :: Ord Int).
--              $csize_a18n3Z @ Int $dOrd_a18n43 $dNum_a18n42
--              = $s$csize_d18mWO]

class Size t where
  size :: t -> t -> Int

instance (Ord a, Num a) => Size [a] where
  {-# SPECIALISE instance Size [Int] #-}
  size (x:xs) (y:ys) | x+y > 4   = size xs ys
                     | otherwise = size xs ys
  size _ _ = 0
