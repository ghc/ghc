module Main(main) where

main = print (take 2 (edi2 0))

-- In 6.12, edi2 lead to a stack overflow (see #3677)

edi :: Integer -> [Integer]
edi x | x `mod` 1000000 == 0 = x : edi (x+1)
      | otherwise             = edi (x+1)

edi2 :: Integer -> [Integer]
edi2 x | x `mod` 1000000 == 0 = x : y
       | otherwise             = y
       where
         y = edi2 (x+1)
