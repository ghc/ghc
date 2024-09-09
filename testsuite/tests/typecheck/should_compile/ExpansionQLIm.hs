module Test where


qqqq :: [String]
qqqq = (show (1 :: Int) :) $ ["2"]

main :: IO ()
main = do
  putStrLn "abc"
  putStrLn $ concat qqqq
