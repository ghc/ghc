import GHC.Float

{-# OPAQUE noinline #-}
noinline :: a -> a
noinline x = x

main :: IO ()
main = do
  -- As per #16617, Word32s should be non-negative
  print $ castFloatToWord32 (-1)
  print $ toInteger (castFloatToWord32 (-1)) > 0
  -- Disable constant folding; see #27300
  print $ castFloatToWord32 (noinline $ -1)
  print $ toInteger (castFloatToWord32 (noinline $ -1)) > 0
  -- For completeness, so should Word64s
  print $ castDoubleToWord64 (-1)
  print $ toInteger (castDoubleToWord64 (-1)) > 0
  print $ castDoubleToWord64 (noinline $ -1)
  print $ toInteger (castDoubleToWord64 (noinline $ -1)) > 0
