
import Control.Concurrent
import System.IO
import System.Mem

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          mapM_ doIter [1..3]

doIter :: Int -> IO ()
doIter n = do putStrLn ("Iteration " ++ show n)
              let xs = [n .. 1000000 + n]
              putStrLn ("Last: " ++ show (last xs))
              putStrLn "GC 1 start"
              performGC
              putStrLn "GC 1 done"
              putStrLn ("Head: " ++ show (head xs))
              putStrLn "GC 2 start"
              performGC
              putStrLn "GC 2 done"

