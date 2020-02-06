import  GHC.Float

main :: IO ()
main = do
   print $ show (word2Double maxBound) == show (fromIntegral (maxBound :: Word) :: Double)
