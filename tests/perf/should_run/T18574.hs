import Data.Maybe

main :: IO ()
main = print $ even $ sum $ catMaybes $ map xx ([1..100000000] :: [Int])
  where
    xx n | even n    = Just n
         | otherwise = Nothing
