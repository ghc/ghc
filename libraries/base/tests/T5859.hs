import Control.Concurrent
import Control.Exception (evaluate)
import Control.Monad
import System.IO.Unsafe

main :: IO ()
main = do
  x <- unsafeInterleaveIO $ putStrLn "eval"
  -- Since this is 'unsafeInterleaveIO' and not 'unsafeDupableInterleaveIO',
  -- this program must print "eval" only once
  replicateM_ 1000 $ forkIO $ evaluate x >> return ()
  threadDelay 1000000
