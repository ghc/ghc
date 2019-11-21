import Numeric.Natural

newtype Mod a = Mod a deriving (Show)

instance Integral a => Num (Mod a) where
  Mod a * Mod b = Mod (a * b         `mod` 10000000019)
  fromInteger n = Mod (fromInteger n `mod` 10000000019)

main :: IO ()
main = do
  -- Should not allocate more compared to Integer
  -- print $ product $ map Mod [(1 :: Integer) .. 100000000]
  print $ product $ map Mod [(1 :: Natural) .. 100000000]
