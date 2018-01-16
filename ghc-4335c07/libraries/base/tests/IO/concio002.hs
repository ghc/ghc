import System.Process
import System.IO
import Control.Concurrent

main = do
  (hin,hout,herr,ph) <- runInteractiveProcess "cat" [] Nothing Nothing
  forkIO $ do threadDelay 100000
              putStrLn "child"
              hFlush stdout
              hPutStrLn hin "msg"
              hFlush hin
  putStrLn "parent1"
  hGetLine hout >>= putStrLn
  putStrLn "parent2"
