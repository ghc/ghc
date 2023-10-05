import Text.Printf

main :: IO ()
main = do
  putStrLn $ printf "%.1f" (0.45 :: Double)
  putStrLn $ printf "%.1f" (0.55 :: Double)
