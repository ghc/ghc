module Main where
main = let zzz = (toEnum (qqq 256)) :: Char
       in  putStrLn (show (zzz == zzz))

-- Only here to defeat potential compile-time evaluation of
-- toEnum applied to literal arg, in excessively clever compilers
qqq :: Int -> Int
qqq 0 = 0
qqq n = 1 + qqq (n-1)

