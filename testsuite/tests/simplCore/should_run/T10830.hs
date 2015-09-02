import GHC.OldList
main :: IO ()
main = maximumBy compare [1..10000] `seq` return ()
