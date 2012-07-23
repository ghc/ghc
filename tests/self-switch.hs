import LwConc.Substrate

main = do
  s <- atomically $ getSCont
  atomically $ switchTo s BlockedInHaskell
  print "Main done"
