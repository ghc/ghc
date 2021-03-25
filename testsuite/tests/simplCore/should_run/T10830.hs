import Data.List (maximumBy)
main :: IO ()
main = maximumBy compare [1..10000] `seq` return ()
