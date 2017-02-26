-- Don't wrap literals that will be used at type Integer
f :: Integer -> Int
f n = case n of
  0x100000000000000000000000 -> 1
  0 -> 2
  _ -> 3

main = print (f (read "0"))
