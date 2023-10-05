-- Test edge cases fixed in
-- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10933

main :: IO ()
main = do
  let
    lo = toInteger (minBound :: Int)
    hi = toInteger (maxBound :: Int)
    readDouble = read :: String -> Double
    readFloat = read :: String -> Float

  print $ readDouble ("1e" <> show lo)
  print $ readDouble ("1e" <> show (lo - 1))
  print $ readDouble ("0.01e" <> show lo)
  print $ readDouble ("10e" <> show hi)

  print $ readFloat ("1e" <> show lo)
  print $ readFloat ("1e" <> show (lo - 1))
  print $ readFloat ("0.01e" <> show lo)
  print $ readFloat ("10e" <> show hi)
