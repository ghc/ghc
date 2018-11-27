x :: String -> String
x s = print (reverse s + 1)

myshow :: (String -> String) -> String
myshow x = show x
