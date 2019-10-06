{-
This test measures the total amount of allocated memory used by reverse.
A first attempt at fusing reverse lead to an increase in memory consumption
in some cases, so it might be better to keep track of that, in case
anybody wants to change these rules.
See Note [Fusing reverse] in libraries/base/GHC/List.hs
-}

module Main where

main :: IO ()
main = do
    let int = 1000000 :: Int
    -- good producer and consumer
    print . foldr (\_ a -> 1 + a :: Int) 0 . reverse $ [1 .. int]
    print . length . reverse $ [1 .. int]
    -- only good consumer
    print . foldr (\_ a -> 1 + a :: Int) 0 . reverse . enum' $ int
    print . length   . reverse . enum' $ int
    -- only good producer
    print . length'  . reverse $ [1 .. int]
    print . length'' . reverse $ [1 .. int]
    -- neiter a good producer or a good consumer
    print . length'' . reverse . enum' $ int

length', length'' :: [a] -> Int
length' []      = 0
length' (_:xs') = 1 + length' xs'

length'' xs = go 0 xs
  where
    go a []      = a
    go a (_:xs') = go (a + 1) xs'

enum' :: Int -> [Int]
enum' x = go 1 x
  where
    go a x
        | a == x = [x]
        | True   = a : go (1 + a) x
