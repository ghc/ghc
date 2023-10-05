-- !!! Testing layout rule
module ShouldCompile where

l1 :: IO ()
l1 = do
  return a
  where
   a = ()

l2 :: IO ()
l2 = do
  return a
 where
  a = ()

l3 :: IO ()
l3 = do
  return a
   where
   a = ()
