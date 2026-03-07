main :: IO ()
main = pure ()

foreign export javascript "my_main" main :: IO ()
