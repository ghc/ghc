main = do
  let x = ()
  res2 <- pure ()
  ~(Just res1) <- seq x (pure $ Nothing @())
  print res1
  print res2
  pure ()
