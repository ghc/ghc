{-# LANGUAGE BangPatterns #-}
data Test = Test !Int

{-# NOINLINE f #-}
f a = Test (a + 1)

main = let (Test x) = f 1 in print x
