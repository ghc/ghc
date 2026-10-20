import Control.Concurrent
import qualified Data.ByteString as B
import System.IO
import System.Process.CommunicationHandle

main :: IO ()
main = do
  (r, ch) <- createWeReadTheyWritePipe
  w <- openCommunicationHandleWrite ch
  mapM_ (`hSetBinaryMode` True) [r, w]
  done <- newEmptyMVar
  _ <- forkIO $ do
    B.hPut w (B.replicate 65536 0)
    hClose w
    putMVar done ()
  s <- B.hGetContents r
  takeMVar done
  print (B.length s)
