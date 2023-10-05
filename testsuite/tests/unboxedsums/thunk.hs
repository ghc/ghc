{-# LANGUAGE UnboxedTuples #-}

{-# NOINLINE f #-}
f :: (# #) -> [Int]
f (# #) = [ 1 .. ]

main :: IO ()
main = print (sum (take 10 (f (# #))))
