module B where

fibonacci :: Int -> Int
fibonacci n =
  if n <= 1
    then 1
    else
      let
        n1 = n - 1
        n2 = n - 2
      in
        fibonacci n1 + fibonacci n2

printFib :: Int -> IO ()
printFib n = do
  let result = fibonacci n
  putStrLn $ show result
