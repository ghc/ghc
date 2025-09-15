module Main where

main :: IO ()
main = do
  a <- foo False undefined
  print a
  print a

foo :: Bool -> Int -> IO Int
foo True  i = return i
foo False _ = do
  let x = 3
      y = 4
  n <- bar (x + y)
  return n

bar :: Int -> IO Int
bar z = do
  let t = z * 2
  y <- foo True t
  return y

