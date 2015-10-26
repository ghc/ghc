testF :: Float -> Bool
testF x = x == 0 && not (isNegativeZero x)

testD :: Double -> Bool
testD x = x == 0 && not (isNegativeZero x)

main :: IO ()
main = do print $ testF (-0.0)
          print $ testD (-0.0)
