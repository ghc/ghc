import Data.Word

main :: IO ()
main = print $ 0x8000000000000000 + zero

zero :: Word64
zero = 0
{-# NOINLINE zero #-}
