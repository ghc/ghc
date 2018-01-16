
main :: IO ()
main = do writeFile "T5536.data" (replicate 10000000 'a')
          readFile "T5536.data" >>= putStr

