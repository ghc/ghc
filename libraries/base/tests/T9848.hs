import Data.IORef

foo :: Int -> Bool
foo n = all (<10000000) [1..n]

bar :: Int -> Bool
bar n = and $ map (<10000000) [1..n]

main :: IO ()
main = do
  ref <- newIORef 1000000
  val <- readIORef ref
  print $ foo val
  print $ bar val
