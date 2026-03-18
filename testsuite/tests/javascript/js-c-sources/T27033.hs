main :: IO ()
main = print =<< increment 41

foreign import javascript "increment_wrapper" increment :: Int -> IO Int
