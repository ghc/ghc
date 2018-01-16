import Data.Bits ((.&.))

flags :: Int -> Int
flags x
  | x .&. 128 > 0 = 12
  | otherwise = 13
{-# NOINLINE flags #-}

main :: IO ()
main = print (flags 255)
