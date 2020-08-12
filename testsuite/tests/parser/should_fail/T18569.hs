main :: IO ()
main = case (# 10, 11 #) of
   (#     foo, bar #) -> print (foo,bar)
