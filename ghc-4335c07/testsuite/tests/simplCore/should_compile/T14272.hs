import Data.Bits (bit)

main :: IO ()
main = putStrLn (show (f undefined))

f :: [Int] -> Int
f = sum . zipWith ((+) . bit) [0..] . map undefined . scanl undefined undefined
