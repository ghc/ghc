module Main where

testfn :: Word -> IO ()
testfn wseq = do
  print $ wseq `mod` 1

main = do
  testfn 5
  print $ (5 :: Word) `mod` 1
