main :: IO ()
main = do
   case 0::Int of
      0 -> putStrLn "A"
      1 -> putStrLn "B"
      _ -> putStrLn "C"

   case 0::Int of
      0 -> putStrLn "A"
      1 -> putStrLn "B"
      2 -> putStrLn "C"

   case 0::Integer of
      0 -> putStrLn "A"
      1 -> putStrLn "B"
      _ -> putStrLn "C"

   case 0::Integer of
      0 -> putStrLn "A"
      1 -> putStrLn "B"
      2 -> putStrLn "C"

   case 0::Integer of
      1 -> putStrLn "B"
      2 -> putStrLn "C"

   case 3::Integer of
      1 -> putStrLn "B"
      2 -> putStrLn "C"
