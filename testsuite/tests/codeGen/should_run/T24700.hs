import Data.Int

main = do
  input <- getLine
  print (read input - 3000000000 :: Int64)
