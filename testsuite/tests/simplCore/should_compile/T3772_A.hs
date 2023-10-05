

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


{-
$cdeepSeq :: DeepSeq a -> [a] -> b -> b
-- User INLINE( 3 args )!
$cdeepSeq a (d:DS a) b (x:[a]) (y:b) = ...

$fDeepSeq[] :: DeepSeq a -> DeepSeq [a]
-- DFun (with auto INLINE pragma)
$fDeepSeq[] a d = $cdeepSeq a d |> blah

$cp1 a d :: C a => DeepSep [a]
-- We don't want to eta-expand this, lest
-- $cdeepSeq gets inlined in it!
$cp1 a d = $fDeepSep[] a (scsel a d)

$fC[] :: C a => C [a]
-- DFun
$fC[] a d = MkC ($cp1 a d) ($cgen a d)
-}