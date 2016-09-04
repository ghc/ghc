main :: IO ()
main = do
  printInt $ 9999 `div` 5
  printInt $ 9999 `mod` 5
  n <- readLn
  printInt $ n `div` 4
  printInt $ n `mod` 4

printInt :: Int -> IO ()
printInt = print
