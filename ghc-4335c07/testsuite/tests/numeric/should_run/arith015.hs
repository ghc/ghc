main = do
  print (map read strange_nums :: [Float]) 
  print (map read strange_nums :: [Double]) 
  where
   strange_nums = ["Infinity","NaN", "-Infinity"]
