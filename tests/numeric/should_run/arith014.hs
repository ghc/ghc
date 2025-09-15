-- Test behaviour of fromInteger when the target type is out of range.

main :: IO ()
main = 
  print [
    fromInteger maxInt2 :: Int,
    fromInteger minInt2 :: Int
  ]  

maxInt2 = fromIntegral (maxBound :: Int)     * 2 :: Integer
minInt2 = fromIntegral (minBound + 1 :: Int) * 2 :: Integer
