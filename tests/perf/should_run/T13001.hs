import Data.IORef

main :: IO ()
main = do
  ref <- newIORef 10000
  n <- readIORef ref
  print $ length $ [0::Int, 2 .. n]
