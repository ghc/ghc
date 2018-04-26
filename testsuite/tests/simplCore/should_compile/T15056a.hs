module T15056a where

test :: Int -> Bool
test 0 = True
test n = test (n-1)

foo :: Foldable t => Int -> t Int -> Int
foo n xs | test n
         = foldr (+) n xs
         | otherwise
         = n+7
