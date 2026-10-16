{-# LANGUAGE MonoLocalBinds #-}
module M where

f :: (Int, Int) -> Int
f p = let {-# NOINLINE a #-}
          (a, b) = p
      in a + b
