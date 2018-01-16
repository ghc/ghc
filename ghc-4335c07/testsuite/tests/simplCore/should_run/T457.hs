f x = case x of
  x@True  -> \y -> x && y
  x@False -> \y -> x && y

main = putStrLn $ f (error "Correct") `seq` "Error"
