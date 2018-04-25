import Foreign.StablePtr

-- compile without optimisation.
-- run with +RTS -D256 to see the stable pointer being garbage collected.

main = do
  let xs = [ 1 .. 50000 ]
  let ys = [ 1 .. 60000 ]
  s1 <- newStablePtr xs
  print (sum xs)
  freeStablePtr s1
  print (sum ys)
