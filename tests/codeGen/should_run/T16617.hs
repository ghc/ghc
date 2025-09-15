import GHC.Float

main :: IO ()
main = do
  -- As per #16617, Word32s should be non-negative
  print $ castFloatToWord32 (-1)
  print $ toInteger (castFloatToWord32 (-1)) > 0
  -- For completeness, so should Word64s
  print $ castDoubleToWord64 (-1)
  print $ toInteger (castDoubleToWord64 (-1)) > 0
