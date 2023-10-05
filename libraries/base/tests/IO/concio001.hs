import Control.Concurrent

main = do
  forkIO $ do threadDelay  100000; putStrLn "child"
  getLine
  putStrLn "parent"
