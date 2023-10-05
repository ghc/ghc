-- Test that floating-point abs works correctly

absF :: Float -> Float
absF = abs

absD :: Double -> Double
absD = abs

main :: IO ()
main = do
    print $ absF (1 / 0)
    print $ absD (1 / 0)
    print $ absF 1
    print $ absD 1
    print $ absF (-1)
    print $ absD (-1)
    print $ absF (-1 / 0)
    print $ absD (-1 / 0)
    print $ absF (0 / 0)
    print $ absD (0 / 0)
    print $ absD $ sqrt (-1)
