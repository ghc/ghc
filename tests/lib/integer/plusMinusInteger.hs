module Main (main) where


main :: IO ()
main = do
    print $ length vals

  where
    boundaries :: [Integer]
    boundaries = [fromIntegral (maxBound :: Int) - 3,
                  fromIntegral (maxBound :: Int) - 2,
                  fromIntegral (maxBound :: Int) - 1,
                  fromIntegral (maxBound :: Int),
                  fromIntegral (maxBound :: Int) + 1,
                  fromIntegral (maxBound :: Int) + 2,
                  fromIntegral (maxBound :: Int) + 3,

                  fromIntegral (minBound :: Int) - 3,
                  fromIntegral (minBound :: Int) - 2,
                  fromIntegral (minBound :: Int) - 1,
                  fromIntegral (minBound :: Int),
                  fromIntegral (minBound :: Int) + 1,
                  fromIntegral (minBound :: Int) + 2,
                  fromIntegral (minBound :: Int) + 3,

                  fromIntegral (maxBound :: Word) - 3,
                  fromIntegral (maxBound :: Word) - 2,
                  fromIntegral (maxBound :: Word) - 1,
                  fromIntegral (maxBound :: Word),
                  fromIntegral (maxBound :: Word) + 1,
                  fromIntegral (maxBound :: Word) + 2,
                  fromIntegral (maxBound :: Word) + 3,

                  -3, -2, -1, 0, 1, 2, 3]
    vals = filter (\(x, y) -> x /= y) [(x - y, x + negate y) |
                                       x <- boundaries, y <- boundaries]
