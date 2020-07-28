import Numeric.Natural

main :: IO ()
main = do
   print $ (0xFFFFFFFF0 * 0xFFFFFFFF0 :: Natural)
   print $ (2 :: Natural) ^ (190 :: Int)
