-- Both sets should fail, without -fglasgow-exts

module ShouldCompile where

  f1 :: Eq a => a -> Bool
  f1 x = (x == x) || g1 True || g1 "Yes"
  
  g1 :: Ord a => a -> Bool
  g1 y = (y <= y) || f1 True

---------

  f2 :: Eq a => a -> Bool
  f2 x = (x == x) || g2 True || g2 "Yes"
  
  g2 y = (y <= y) || f2 True
