main = putStr (shows (f (read "42.0")) "\n")

-- f compiled to bogus code with ghc 0.18 and earlier
-- switch() on a DoubleReg

f :: Double -> Int
f 1.0 = 1
f 2.0 = 2
f 3.0 = 3
f x = round x
