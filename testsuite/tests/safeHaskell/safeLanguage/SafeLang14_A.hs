module SafeLang14_A (IsoInt, h, showH, P, p, showP) where

newtype IsoInt = I Int

h :: IsoInt
h = I 2

showH :: String
showH = let I n = h
        in show n

data P = P Int

p :: P
p = P 3

showP :: P -> String
showP (P n) = "Should be 3 := " ++ show n

