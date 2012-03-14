
main :: IO ()
main = do f "100e12"
          f "00123.456"

f :: String -> IO ()
f str = print (reads str :: [(Integer, String)])
