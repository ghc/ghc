
main :: IO ()
main = do f "Infinity"
          f "-Infinity"
          f "  -  Infinity  "
          f "NaN"
          f "-NaN"
          f "  -  NaN  "

f :: String -> IO ()
f str = print (reads str :: [(Double, String)])
