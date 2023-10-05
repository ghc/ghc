import Data.Word( Word8 )

toV :: Float -> Word8
toV d = let coeff = significand d *  255.9999 / d
            a = truncate $ d * coeff
            b = exponent d
        in a `seq` (b `seq` a)

main :: IO ()
main =
  print $ map toV [ 3.56158e-2, 0.7415215, 0.5383201, 0.1289829, 0.45520145 ]
