føb :: Integer -> Integer
føb n
  | n == 0 = 0
  | n == 1 = 1
  | n >= 2 = føb (n - 1) + føb (n - 2)

main :: IO ()
main = print (føb 26)
