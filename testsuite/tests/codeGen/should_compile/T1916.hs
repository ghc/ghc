module Bug (tst) where
tst :: Float -> Bool
tst x = truncate x > (0::Int)
