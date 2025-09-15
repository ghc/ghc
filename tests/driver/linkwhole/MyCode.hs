module MyCode
  ( myFunction
  ) where

myFunction :: Int -> IO ()
myFunction i = putStrLn $ "Adding to 20: " ++ show (i + 20)
