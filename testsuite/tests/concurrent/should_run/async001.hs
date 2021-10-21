import Control.Exception as E
import Control.Concurrent
import System.IO.Unsafe

-- x is killed during evaluation with an asynchronous exception, but
-- nevertheless gets overwritten with 'throw ThreadKilled' because the
-- async exception is re-thrown as a synchrnonous exception by
-- 'onException'.

main = do
  let x = unsafePerformIO $
             (do threadDelay 1000000; return 42)
             `onException` return ()

  t <- forkIO $ do evaluate x; return ()
  threadDelay 1000
  killThread t

  print x `E.catch` \e -> putStrLn ("main caught: " ++ show (e::SomeExceptionWithLocation))
