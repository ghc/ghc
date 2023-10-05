import Control.Concurrent
import Control.Exception
import System.IO
import System.Posix
import System.Posix.IO

main = do
  (pout1, pin1) <- createPipe
  (pout2, _) <- createPipe
  pid <- forkProcess $ do
           hdl <- fdToHandle pin1
           hSetBuffering hdl LineBuffering
           handle (\UserInterrupt{} -> hPutStrLn hdl "caught")
                  $ do hPutStrLn hdl "registered"
                       hdl2 <- fdToHandle pout2
                       putStrLn =<< hGetLine hdl2
  hdl <- fdToHandle pout1
  hSetBuffering hdl LineBuffering
  "registered" <- hGetLine hdl
  signalProcess sigINT pid
  putStrLn =<< hGetLine hdl
