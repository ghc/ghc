{-# LANGUAGE ImplicitParams #-}
module T8474 where

data D = D Int deriving Show

-- In 7.7 this took exponential time!
slow_to_compile :: IO ()
slow_to_compile = do
  tst1 <- return 1

  let ?tst1 = tst1
  let ?tst2 = tst1
  let ?tst3 = tst1
  let ?tst4 = tst1
  let ?tst5 = tst1
  let ?tst6 = tst1
  let ?tst7 = tst1

  print $ D ?tst1