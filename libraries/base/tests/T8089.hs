import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Environment
import System.Exit
import System.Process
import System.Timeout

testLoop :: Int -> IO (Maybe a) -> IO (Maybe a)
testLoop 0 _ = return Nothing
testLoop i act = do
    result <- act
    case result of
        Nothing -> threadDelay 100000 >> testLoop (i-1) act
        Just x -> return (Just x)


forkTestChild :: IO ()
forkTestChild = do
    (_, _, _, hnd) <- createProcess (proc "./T8089" ["test"])
    result <- testLoop 50 $ getProcessExitCode hnd
    case result of
        Nothing -> terminateProcess hnd >> exitSuccess
        Just exitCode -> exitWith exitCode

main :: IO ()
main = do
    numArgs <- length <$> getArgs
    if numArgs > 0
       then threadDelay maxBound
       else forkTestChild
