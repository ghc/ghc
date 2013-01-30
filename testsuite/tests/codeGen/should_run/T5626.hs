module Main where

wrap x = [x]!!0

f :: [Int] -> a
f a = foldr (\b -> \c -> c) (undefined ()) (a ++ a) 0

main = do
  print $ (f [] :: String)
  print $ wrap $ (f [] :: Int)
  print $ wrap $ (f [] :: (Int, Int, Int, Int))
