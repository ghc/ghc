import LwConc.Substrate

main = do
  let task = print "TASK DONE"
  s <- newBoundSCont task
  putStrLn "Main done"
