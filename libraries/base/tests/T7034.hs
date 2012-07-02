main :: IO ()
main = do
  print $ r "1E100000"
  print $ r "1E100000000"
  print $ r "1E100000000000"
  print $ r "1E100000000000000"
  print $ r "1E100000000000000000"
  print $ r "1E100000000000000000000"

r :: String -> Double
r = read
