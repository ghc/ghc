import Data.Ratio

main = do
  print (approxRational (0 % 1 :: Ratio Int) (1 % 10)) -- 0%1, correct
  print (approxRational (0 % 1 :: Ratio Word) (1 % 10)) -- 1%1, incorrect
