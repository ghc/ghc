

module T3772_A where

class DeepSeq a => C a where
-- class C a where
  gen :: Int -> a

instance C Double where
  gen = fromIntegral

instance C a => C [a] where
  {-# INLINE gen #-}
  gen n = replicate n (gen n)

class DeepSeq a where
  deepSeq :: a -> b -> b

instance DeepSeq Double where
  deepSeq = seq

instance DeepSeq a => DeepSeq [a] where
  {-# INLINE deepSeq #-}
  deepSeq xs b = foldr deepSeq b xs

apply :: (C a, DeepSeq b) => Int -> (a -> b) -> ()
{-# INLINE apply #-}
apply n f = f (gen n) `deepSeq` ()


