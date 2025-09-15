module T21144 where

peps :: a ~ Double => a
peps = x where x = fromIntegral (floatDigits x) ** 2
