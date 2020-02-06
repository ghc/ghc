import  GHC.Float

main :: IO ()
main = do
   print (word2Double maxBound)
   print (fromIntegral (maxBound :: Word) :: Double)
