--!!! do & where interaction
module ShouldSucceed where

f1 :: IO a -> IO [a]
f1 x = do
  v <- x
  return [v]
 where
  g x = [x,x]

f2 :: IO a -> IO [a]
f2 x = do
  v <- x
  return (g v)
   where
    g x = [x,x]

f3 :: IO a -> IO [a]
f3 x = do
  v <- x
  return (g v)
  where
   g x = [x,x]

