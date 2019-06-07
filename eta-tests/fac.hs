fac :: Int -> Int -> Int
fac = \a -> \n ->
  case n of
    0 -> a
    n -> fac (a * n) (n - 1)

main :: IO ()
main = print (fac 1 5 + 25)
