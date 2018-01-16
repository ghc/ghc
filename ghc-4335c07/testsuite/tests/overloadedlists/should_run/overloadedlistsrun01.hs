{-# LANGUAGE OverloadedLists #-}

main = do print ([] :: [Int])
          print ([1,2,3] :: [Int])
          print ((take 10 [1..]) :: [Int])
          print (['a'..'e'] :: [Char])
