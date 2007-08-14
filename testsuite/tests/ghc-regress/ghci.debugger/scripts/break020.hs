line1 _ = return ()
line2 _ = return ()

schloss = do
  line1 1
  line2 2
  return ()

main = do
  line1 (ex1 1)
  line2 (ex2 2)
  schloss
  line2 1
  return ()
  
  
ex1 x = x
ex2 y = y