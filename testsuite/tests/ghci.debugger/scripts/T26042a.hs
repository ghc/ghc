module Main where

main :: IO ()
main = do
  a <- foo
  print a

foo :: IO Int
foo = do
  let x = 3
      y = 4
  b <- bar (x + y)
  return b

bar :: Int -> IO Int
bar z = return (z * 2)

