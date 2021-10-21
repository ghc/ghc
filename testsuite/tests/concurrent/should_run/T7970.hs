import Control.Exception as E
import Data.IORef
import System.Mem.Weak
import Control.Concurrent

main :: IO ()
main = do
    ref <- newIORef 'x'
    weak <- mkWeakIORef ref $ putStrLn "IORef finalized"
    let check = deRefWeak weak >>= \m -> case m of
            Nothing -> putStrLn "IORef was GCed"
            Just ref' -> do
                x <- readIORef ref'
                putStrLn $ "IORef still alive, and contains " ++ show x
    m <- newEmptyMVar
    check
    takeMVar m `catch` \ex -> do
        putStrLn $ "caught exception: " ++ show (ex :: SomeExceptionWithLocation)
        check
    readIORef ref >>= print
