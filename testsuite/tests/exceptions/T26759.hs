import Control.Exception

run :: IO ()
run = failingAction `onException` failingCleanup
  where
    failingAction = throwIO (ErrorCall "outer failure")
    failingCleanup = throwIO (ErrorCall "cleanup failure")

main :: IO ()
main = run
