import T10971c

main = do
  print $ f (Just 1)
  print $ g (+1) (Just 5)
  print $ h (const 5) Nothing
