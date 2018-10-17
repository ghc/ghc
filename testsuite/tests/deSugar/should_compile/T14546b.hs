main :: IO ()
main = do
   case "foo" of
      ('f':_)     -> putStrLn "A"
      ('f':'o':_) -> putStrLn "B"
      "bar"       -> putStrLn "C"

   case "foo" of
      "foo" -> putStrLn "A"
      "bar" -> putStrLn "B"
      "baz" -> putStrLn "C"
