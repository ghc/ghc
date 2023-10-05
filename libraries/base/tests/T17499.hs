import Numeric.Natural

import Control.Exception (evaluate)

newtype Mod a = Mod a deriving (Show)

instance Integral a => Num (Mod a) where
  Mod a * Mod b = Mod (a * b         `mod` 10000000019)
  fromInteger n = Mod (fromInteger n `mod` 10000000019)

main :: IO ()
main = do
  -- Should not allocate more compared to Integer
  -- _ <- evaluate $ product $ map Mod [(1 :: Integer) .. 1000000]
  _ <- evaluate $ product $ map Mod [(1 :: Natural) .. 1000000]
  return ()
