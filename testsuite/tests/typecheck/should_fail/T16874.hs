
module Main where

type A = Int
data D = D A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A A

test :: D -> D
test (D a b c d e f g h i j k l m n o p q r s t u v w x y z aa bb cc dd ee ff gg hh ii jj kk ll mm nn)
  = D a b c d e f g h i j k l m n o p q r s t u v w x y z aa bb cc dd ee ff gg hh ii jj kk ll mm nn

main :: IO ()
main = print ()
