main :: IO ()
main = do
  -- force an "unrecognized pragma" warning
  -- to check if the column number is correct
  {-# COLUMN 1000 #-}print "Hello"  {-# NONEXISTENTPRAGMA #-}
  print "world"
