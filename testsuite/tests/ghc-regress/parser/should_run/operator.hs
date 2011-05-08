module Main where

data Hash = Hash { (#) :: Int }
  deriving (Show,Read)

main = do
  let h = Hash 3
      s = show h
      x = read s :: Hash
      y = show x
  print h
  putStrLn s
  print x
  putStrLn y
  let h' = Hash { (#) = 3 }
      s' = show h'
      x' = read s' :: Hash
      y' = show x'
  print h'
  putStrLn s'
  print x'
  putStrLn y'

