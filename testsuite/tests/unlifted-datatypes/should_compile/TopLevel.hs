import TopLevela

toInt UZero = 0
toInt (USucc x) = 1 + toInt x

main = case x of
  Box y -> print (toInt y)
