module T16804b where

import T16804a

printStuff :: IO ()
printStuff = do
  print (testFunction A A)
  print (testFunction2 True)
  print (testFunction2 False)
  print niceValue
