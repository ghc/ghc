main :: IO ()
main = do
    print $ (asinh 0 :: Double)
    print $ (atanh (-0) :: Double)
    print $ relError 691.4686750787736 (asinh 1e300 :: Double) < 1e-9

relError :: Double -> Double -> Double
relError expected actual = (actual - expected) / expected
