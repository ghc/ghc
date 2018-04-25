
import Data.List

main :: IO ()
main = print $ length $ show (foldl' (*) 1 [1..100000] :: Integer)
