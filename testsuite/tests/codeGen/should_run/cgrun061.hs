
module Main where

newtype Test = Test { var :: String }

{-
hugs (Sept 2006) gives
Program error: pattern match failure: instShow_v16_v1443 (Test_Test "a")
Program error: pattern match failure: instShow_v16_v1443 (Test_Test "b")
hugs trac #46
-}

main = do print (var x)
          print (var (y{var="b"}))
    where x = Test { var = "a" }
          y = Test "a"

