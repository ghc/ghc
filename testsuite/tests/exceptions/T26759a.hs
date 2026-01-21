import Control.Exception

run :: IO ()
run = failingAction `onException` cleanup
  where
    failingAction = throwIO (ErrorCall "outer failure")
    cleanup = putStrLn "cleanup"

main :: IO ()
main = run
